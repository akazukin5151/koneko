"""prompt.py but for lscat_app"""

import os
import time
from abc import ABC, abstractmethod

from pixcat import Image

from koneko import (
    pure,
    TERM,
    utils,
    lscat,
    config,
    picker,
    printer,
    FakeData,
    WELCOME_IMAGE,
)


# Constants
PLUS = frozenset({'+', '='})
MINUS = frozenset({'-', '_'})


def thumbnail_size_assistant() -> 'IO[int]':
    """=== Thumbnail size ===
    This will display an image whose thumbnail size can be varied
    Use +/= to increase the size, and -/_ to decrease it
    Use q to exit the program, and press enter to confirm the size

    Keep in mind this size will be used for a grid of images
    """
    # Setup
    images = []
    size = 300
    previous_size = 300

    # Start
    printer.print_doc(thumbnail_size_assistant.__doc__)

    with TERM.cbreak():
        while True:
            images.append(lscat.api.show(WELCOME_IMAGE, 0, 0, size))
            with TERM.location(0, (TERM.height - 10)):
                print(f'size = {size}')

            ans = TERM.inkey()
            utils.quit_on_q(ans)

            if ans in PLUS:
                previous_size = size
                size += 20

            # +, +, +, -, +
            # |     |  ^  |> Do nothing here as well
            # |     |  |
            # |     |  |
            # |     |  This is where all images should be hidden
            # |     |
            # No images hidden in this range
            elif ans in MINUS:
                previous_size = size
                size -= 20
                if previous_size > size:
                    lscat.api.hide_all(images)

            elif ans.name == 'KEY_ENTER':
                lscat.api.hide_all(images)
                return size


def xpadding_assistant(thumbnail_size: int) -> 'IO[int]':
    """=== Image x spacing ===
    1) Move the second image so that it is just to the right of the first image
       Use +/= to move it to the right, and -/_ to move it to the left.
       Press enter to confirm

    2) Based on the position of the second image, adjust its position to suit you.
       This value will be the x spacing

    Use q to exit the program, and press enter to go to the next assistant
    """
    return _XPadding(thumbnail_size).start()


def ypadding_assistant(thumbnail_size: int) -> 'IO[int]':
    """=== Image y spacing ===
    1) Move the second image so that it is just to the bottom of the first image
       Use +/= to move it downwards, and -/_ to move it upwards.
       Press enter to confirm

    2) Based on the current height of the second image, adjust its height to suit you.
       This value will be the y spacing

    Use q to exit the program, and press enter to go to the next assistant
    """
    return _YPadding(thumbnail_size).start()


class _AbstractImageAdjuster(ABC):
    def __init__(self):
        # Defined in child classes
        self.thumbnail_size: int
        self.show_func: 'func'
        self.side_label: str
        self.start_spaces: int

        # Defined in start()
        self.image: Image
        self.width_or_height: int
        self.spaces: int
        self.valid: bool

    @abstractmethod
    def report(self) -> 'IO':
        """Report the result to be returned to the user by writing to stdout"""
        raise NotImplementedError

    def maybe_move_up(self) -> 'IO':
        return True

    def maybe_move_down(self) -> 'IO':
        return True

    def maybe_erase(self) -> 'IO':
        return True

    @abstractmethod
    def show_func_args(self) -> Image:
        """Show pixcat image, where the function and its args can be customized
        Returns a reference of that Image, so it can be hidden later
        """
        raise NotImplementedError

    @abstractmethod
    def return_tup(self) -> (int, int):
        """Final return values after the completion of the assistant"""
        raise NotImplementedError

    @abstractmethod
    def is_input_valid(self) -> bool:
        """Check if user input is valid"""
        raise NotImplementedError

    def hide_show_print(self) -> 'IO':
        """Hide image if shown, show another image, and report"""
        if self.image:
            lscat.api.hide(self.image)

        self.image = self.show_func_args()

        self.maybe_move_up()
        printer.write('\r' + ' ' * 20 + '\r')
        with TERM.location(0, TERM.height - 13):
            self.report()

    def start(self) -> (int, int):
        """Main loop"""
        self.maybe_move_down()

        self.spaces = self.start_spaces
        self.valid = True

        with TERM.cbreak():
            while True:
                if self.is_input_valid():
                    self.hide_show_print()

                ans = TERM.inkey()
                utils.quit_on_q(ans)

                if ans.name == 'KEY_ENTER' and self.image:
                    self.maybe_erase()
                    lscat.api.hide(self.image)
                    if self.static:
                        lscat.api.hide(self.static)
                    return self.return_tup()

                if ans in PLUS:
                    self.spaces += 1
                    self.valid = True

                elif ans in MINUS and self.spaces > self.start_spaces:
                    self.spaces -= 1
                    self.valid = True

                else:
                    self.valid = False


class _AbstractPadding(_AbstractImageAdjuster, ABC):
    def __init__(self):
        # Base
        self.start_spaces = 0

        # New attributes
        self.doc: str
        self.default_x: int
        self.find_dim_func: 'func'

    def return_tup(self) -> (int, int):
        """Implements abstractmethod"""
        lscat.api.hide(self.image)
        return self.spaces, self.width_or_height

    def is_input_valid(self) -> bool:
        """Implements abstractmethod: one condition"""
        return bool(self.valid)

    def start(self) -> (int, int):
        """Complements concrete method: Find image width/height first"""
        printer.print_doc(self.doc)

        self.static = lscat.api.show(
            WELCOME_IMAGE, self.default_x, 0, self.thumbnail_size
        )

        self.width_or_height, self.image = self.find_dim_func(
            self.thumbnail_size,
        ).start()

        return super().start()


class _XPadding(_AbstractPadding):
    def __init__(self, thumbnail_size):
        super().__init__()
        # Base
        self.thumbnail_size = thumbnail_size
        self.show_func = lscat.show_single_x
        self.side_label = 'width'

        # Padding ABC
        self.doc = xpadding_assistant.__doc__
        self.default_x = config.xcoords_config()[0]
        self.find_dim_func = _FindImageWidth

    def report(self) -> 'IO':
        """Implements abstractmethod"""
        # The trailing spaces at the end ensures everything is covered after exit
        printer.write(f'x spacing = {self.spaces}    ')

    def show_func_args(self) -> Image:
        """Implements abstractmethod: First argument is unique"""
        return self.show_func(
            self.default_x + self.width_or_height + self.spaces, self.thumbnail_size
        )


class _YPadding(_AbstractPadding):
    def __init__(self, thumbnail_size):
        super().__init__()
        # Base
        self.thumbnail_size = thumbnail_size
        self.show_func = lscat.show_single_y
        self.side_label = 'height'

        # Padding ABC
        self.doc = ypadding_assistant.__doc__
        self.default_x = config.xcoords_config()[1]
        self.find_dim_func = _FindImageHeight

        # Unique
        self.use_ueberzug = not config.use_ueberzug()

    def report(self) -> 'IO':
        """Implements abstractmethod"""
        printer.write(f'y spacing = {self.spaces}      ')
        # The trailing spaces at the end ensures everything is covered after exit

    def maybe_move_down(self) -> 'IO':
        """Overrides base method: move cursor down"""
        if self.use_ueberzug:
            printer.move_cursor_down(self.width_or_height)

    def maybe_move_up(self) -> 'IO':
        """Overrides base method: move cursor up"""
        if self.use_ueberzug:
            printer.move_cursor_up(self.spaces)

    def show_func_args(self) -> Image:
        """Implements abstractmethod: first argument is unique"""
        return self.show_func(self.width_or_height + self.spaces, self.thumbnail_size)


class _FindImageDimension(_AbstractImageAdjuster, ABC):
    def __init__(self):
        # Base
        # Defined in child classes
        self.thumbnail_size: int
        self.show_func: 'func'
        self.side_label: str
        self.start_spaces: int
        self.image = None
        self.static = None

    def report(self) -> 'IO':
        """Implements abstractmethod"""
        printer.write(f'image {self.side_label} = {self.spaces - self.start_spaces}')

    def maybe_erase(self) -> 'IO':
        """Overrides base method: erase line"""
        printer.erase_line()

    def show_func_args(self) -> Image:
        """Implements abstractmethod: first argument is unique"""
        return self.show_func(self.spaces, self.thumbnail_size)

    def return_tup(self) -> (int, int):
        """Implements abstractmethod"""
        return self.spaces - self.start_spaces, self.image

    def is_input_valid(self) -> bool:
        """Implements abstractmethod: two conditions"""
        return self.spaces >= self.start_spaces and self.valid


class _FindImageWidth(_FindImageDimension):
    def __init__(self, thumbnail_size):
        self.thumbnail_size = thumbnail_size
        self.show_func = lscat.show_single_x
        self.side_label = 'width'
        self.start_spaces = config.xcoords_config()[0]
        super().__init__()


class _FindImageHeight(_FindImageDimension):
    def __init__(self, thumbnail_size):
        self.thumbnail_size = thumbnail_size
        self.show_func = lscat.show_single_y
        self.side_label = 'height'
        self.start_spaces = 0
        super().__init__()

    def maybe_move_up(self) -> 'IO':
        """Overrides base method: move cursor up"""
        printer.move_cursor_up(self.spaces)


def check_ueberzug() -> bool:
    if config.use_ueberzug():
        os.system('clear')
        print(
            'The page spacing assistant is not needed if you use ueberzug, '
            'because ueberzug does not respond to scroll events'
        )
        return False
    return True


def page_spacing_assistant(thumbnail_size: int) -> int:
    # This doesn't use print_doc() as a clean state is needed
    if not check_ueberzug():
        return None

    os.system('clear')
    print(
        *(
            '=== Page spacing ===',
            'This will display an image, then print newlines.',
            'Your desired setting is the number when '
            'the image completely scrolls out of view',
        ),
        sep='\n',
    )

    input('\nEnter any key to continue\n')
    os.system('clear')

    image = lscat.api.show(WELCOME_IMAGE, 0, 0, thumbnail_size)

    time.sleep(0.1)

    for i in range(TERM.height + 5):
        print(i)
        time.sleep(0.1)

    print(
        'When the image just completely scrolls out of view, '
        'what is the largest number?'
    )
    print('(By default on kitty, ctrl+shift+up/down ' 'scrolls up/down a line)')

    while True:
        ans = input()
        if ans.isdigit():
            lscat.api.hide(image)
            return ans
        print('Must enter a number!')


def _display_inital_row(ans, size, xpadding, image_width, image_height):
    if ans == 'y':
        _path = picker.pick_dir()
        _data = FakeData(_path)
        images = lscat.handle_scroll(lscat.TrackDownloads, _data, slice(None))
        ncols = config.ncols_config()  # Default fallback, on user choice
        if config.use_ueberzug():
            print('\n' * (image_height * config.nrows_config() + 1))
        return ncols, images

    images = lscat.api.show_row(WELCOME_IMAGE, xpadding, image_width, size)
    ncols = pure.ncols(TERM.width, xpadding, image_width)
    if config.use_ueberzug():
        print('\n' * (image_height - 2))
    return ncols, images


def gallery_print_spacing_assistant(size, xpadding, image_width, image_height: int) -> 'list[int]':
    """=== Gallery print spacing ===
    Use +/= to increase the spacing, and -/_ to decrease it
    Use q to exit the program, and press enter to go to the next assistant
    Use left and right arrow keys to change the current space selection

    Do you want to preview an existing cache dir? [y/N]
    To keep your chosen thumbnail size, image width and x spacing, enter 'n'.
    """
    printer.print_doc(gallery_print_spacing_assistant.__doc__)  # Action before start
    ans = input()

    # Setup variables
    ncols, images = _display_inital_row(ans, size, xpadding, image_width, image_height)

    # Just the default settings; len(first_list) == 5
    spacings = [9, 17, 17, 17, 17] + [17] * (ncols - 5)
    current_selection = 0

    # Start
    print('\n')
    with TERM.cbreak():
        while True:
            printer.update_gallery_info(spacings, ncols, current_selection)

            ans = TERM.inkey()
            utils.quit_on_q(ans)

            if ans in PLUS and pure.line_width(spacings, ncols) < TERM.width:
                spacings[current_selection] += 1

            elif ans in MINUS and spacings[current_selection] > 0:
                spacings[current_selection] -= 1

            # right arrow
            elif (
                ans.name == 'KEY_RIGHT'
                or ans in {'d', 'l'}
                and current_selection < len(spacings) - 1
            ):
                current_selection += 1

            # left arrow
            elif ans.name == 'KEY_LEFT' or ans in {'a', 'h'} and current_selection > 0:
                current_selection -= 1

            elif ans.name == 'KEY_ENTER':
                lscat.api.hide_all(images)
                return spacings


def user_info_assistant(thumbnail_size, xpadding, image_width: int) -> int:
    """=== User print name xcoord ===
    Use +/= to move the text right, and -/_ to move it left
    Adjust the position as you see fit

    Use q to exit the program, and press enter to confirm the current position
    """
    # Setup variables
    spacing, _ = config.get_gen_users_settings()  # Default
    preview_xcoords = pure.xcoords(TERM.width, image_width, xpadding, 1)[-3:]

    # Start
    printer.print_doc(user_info_assistant.__doc__)

    images = lscat.api.show_user_row(
        WELCOME_IMAGE, preview_xcoords, xpadding, thumbnail_size
    )

    if not config.use_ueberzug():
        printer.move_cursor_up(5)
    else:
        printer.move_cursor_down(3)

    with TERM.cbreak():
        while True:
            printer.update_user_info(spacing)

            ans = TERM.inkey()
            utils.quit_on_q(ans)

            if ans in PLUS:
                spacing += 1

            elif ans in MINUS and spacing > 0:
                spacing -= 1

            elif ans.name == 'KEY_ENTER':
                print('\n' * 3)
                lscat.api.hide_all(images)
                return spacing


def center_spaces_assistant():
    """=== Ueberzug center image ===
    Use +/= to move the image right, and -/_ to move it left
    Adjust the position to the center of your terminal

    Use q to exit the program, and press enter to confirm the current position
    """
    if not config.use_ueberzug():
        print('Center images assistant is only for ueberzug!')
        return -1

    # Setup
    image = WELCOME_IMAGE.parent / '79494300_p0.png'
    valid = True
    spacing = 0
    i = 0

    # Start
    printer.print_doc(center_spaces_assistant.__doc__)
    print('\n' * (TERM.height - 9), f'Current position: {spacing:01}')

    with TERM.cbreak():
        while True:
            if valid:
                printer.move_cursor_up(1)
                print(f'Current position: {spacing:02}')
                placement = lscat.api.show(image, spacing, 0, 500)

            ans = TERM.inkey()
            utils.quit_on_q(ans)

            if ans in PLUS:
                spacing += 1
                valid = True

            elif ans in MINUS and spacing > 0:
                spacing -= 1
                valid = True

            elif ans.name == 'KEY_ENTER':
                os.system('clear')
                lscat.api.hide(placement)
                return spacing

            else:
                valid = False

            if valid:
                lscat.api.hide(placement)
                i += 1
