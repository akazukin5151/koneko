import os
import sys
import time
from copy import copy
from pathlib import Path
from abc import ABC, abstractmethod

from pixcat import Image
from blessed import Terminal

from koneko import KONEKODIR, pure, lscat, config


# Globals
term = Terminal()
# Must make a copy before using this reference
SAMPLE_IMAGE = Image(KONEKODIR.parent / 'pics' / '71471144_p0.png')
PLUS = {'+', '='}
MINUS = {'-', '_'}
ENTER = 343


# Utility functions used in multiple places
def write(string):
    print(string, end='', flush=True)

def check_quit(ans):
    if ans == 'q':
        sys.exit(0)

def move_cursor_up(num):
    if num > 0:
        write(f'\033[{num}A')

def move_cursor_down(num=1):
    if num > 0:
        write(f'\033[{num}B')

def erase_line():
    write('\033[K')

def print_cols(spacings, ncols):
    for (idx, space) in enumerate(spacings[:ncols]):
        write(' ' * int(space))
        write(idx + 1)

def line_width(spacings, ncols):
    return sum(spacings) + ncols

def print_doc(doc):
    """Prints a given string in the bottom of the terminal"""
    os.system('clear')
    number_of_newlines = doc.count('\n')
    bottom = term.height - (number_of_newlines + 2)
    move_cursor_down(bottom)
    print(doc)


# More specialised but still small functions
class FakeData:
    def __init__(self, path):
        self.download_path = path

    @classmethod
    def gallery(cls):
        return cls(KONEKODIR / 'testgallery')

    @classmethod
    def user(cls):
        # It needs to have a .koneko file
        return cls(KONEKODIR / 'testuser')


def print_info(message_xcoord):
    print(' ' * message_xcoord, '000', '\n',
          ' ' * message_xcoord, 'Example artist', sep='')

def show_single(x, y, thumbnail_size):
    img = copy(SAMPLE_IMAGE).thumbnail(thumbnail_size)
    img.show(align='left', x=x, y=y)
    return img

def show_single_x(x, thumbnail_size):
    return show_single(x, 0, thumbnail_size)

def show_single_y(y, thumbnail_size):
    # Default usage of config module
    return show_single(config.xcoords_config()[1], y, thumbnail_size)

def show_instant_sample(thumbnail_size, xpadding, image_width):
    xcoords = pure.xcoords(term.width, image_width, xpadding)
    for x in xcoords:
        show_single(x, 0, thumbnail_size)

def display_user_row(thumbnail_size, preview_xcoords, padding):
    show_single(padding, 0, thumbnail_size)
    for px in preview_xcoords:
        show_single_x(px, thumbnail_size)

def hide_if_exist(image):
    if image:
        image.hide()
        move_cursor_up(1)


# Main functions that organise work
def main():
    os.system('clear')
    print(*('Welcome to the lscat interactive script',
        '1. Launch koneko configuration assistance',
        '2. Display KONEKODIR / testgallery',
        '3. Display KONEKODIR / testuser',
        '4. Browse a cached dir to display',
        '5. Display a specified path'), sep='\n')

    ans = input('\nPlease select an action: ')
    print('')

    case = {
        '1': config_assistance,
        '2': display_gallery,
        '3': display_user,
        '4': browse_cache,
        '5': display_path
    }

    func = case.get(ans, None)
    if func:
        func()
    else:
        print('Invalid command! Exiting...')


def display_gallery():
    data = FakeData.gallery()
    lscat.show_instant(lscat.TrackDownloads, data, True)

def display_user():
    data = FakeData.user()
    lscat.show_instant(lscat.TrackDownloadsUsers, data)

def display_path():
    path = input('Please paste in your path:\n')
    if not Path(path).is_dir():
        print('Invalid path!')
        sys.exit(1)

    data = FakeData(path)
    lscat.show_instant(lscat.TrackDownloads, data, True)


def browse_cache():
    path = pick_dir()
    data = FakeData(path)

    ans = input('Does this directory have a .koneko file? [y/N] ')

    if ans == 'n':
        lscat.show_instant(lscat.TrackDownloads, data, True)
    else:
        lscat.show_instant(lscat.TrackDownloadsUsers, data)

def pick_dir():
    path = KONEKODIR

    while True:
        files = sorted(os.listdir(path))
        for i, f in enumerate(files):
            print(i, '--', f)

        print('\nSelect a directory to view (enter its index)')
        print('If you want to display this directory, enter "y"')
        print("Enter 'b' to move up a directory")
        ans = input()
        check_quit(ans)

        if ans == 'y':
            return path

        elif ans == 'b':
            path = path.parent
            continue

        if ans.isdigit():
            path = path / files[int(ans)]
        else:
            print('Invalid command!')



def config_assistance():
    """Some assistants return a new setting, which should be propagated
    to other assistants.
    """
    print(*('\n=== Configuration assistance ===',
        'Please select an action index',
        '1. Thumbnail size',
        '2. x-padding',
        '3. y-padding',
        '4. Page spacing',
        '5. Gallery print spacing',
        '6. User mode print info x-position',
        'a. (Run all of the above)\n'), sep='\n')
    ans = input()

    if ans in {'1', 'a'}:
        size = thumbnail_size_assistant()
    else:
        size = config.thumbnail_size_config()  # Fallback

    if ans in {'2', 'a'}:
        xpadding, image_width = xpadding_assistant(size)
    else:
        # Fallbacks
        _, xpadding = config.get_gen_users_settings()
        image_width, _ = config._width_padding('width', 'x', (0, 2))

    if ans in {'3', 'a'}:
        ypadding, image_height = ypadding_assistant(size)

    if ans in {'4', 'a'}:
        page_spacing = page_spacing_assistant(size)

    if ans in {'5', 'a'}:
        gallery_print_spacing = gallery_print_spacing_assistant(
            size, xpadding, image_width
        )

    if ans in {'6', 'a'}:
        user_info_xcoord = user_info_assistant(
            size,
            xpadding,
            image_width
        )


    print('\n\nYour recommended settings are:')
    if ans in {'1', 'a'}:
        print(f'image_thumbnail_size = {size}')

    if ans in {'2', 'a'}:
        print(f'image_width = {image_width}')
        print(f'images_x_spacing = {xpadding}')

    if ans in {'3', 'a'}:
        print(f'image_height = {image_height}')
        print(f'images_y_spacing = {ypadding}')

    if ans in {'4', 'a'}:
        print(f'page_spacing = {page_spacing}')

    if ans in {'5', 'a'}:
        print(f'gallery_print_spacing =',
              ','.join((str(x) for x in gallery_print_spacing)))

    if ans in {'6', 'a'}:
        print(f'users_print_name_xcoord = {user_info_xcoord}')

    input('\nEnter any key to quit\n')


def thumbnail_size_assistant():
    """=== Thumbnail size ===
    This will display an image whose thumbnail size can be varied
    Use +/= to increase the size, and -/_ to decrease it
    Use q to exit the program, and press enter to confirm the size

    Keep in mind this size will be used for a grid of images
    """
    print_doc(thumbnail_size_assistant.__doc__)

    image = copy(SAMPLE_IMAGE)

    size = 300  # starting size
    with term.cbreak():
        while True:
            image.thumbnail(size).show(align='left', x=0, y=0)

            ans = term.inkey()
            check_quit(ans)

            if ans in PLUS:
                size += 20

            elif ans in MINUS:
                image.hide()
                size -= 20

            elif ans.code == ENTER:
                return size


def xpadding_assistant(thumbnail_size):
    """=== Image x spacing ===
    1) Move the second image so that it is just to the right of the first image
       Use +/= to move it to the right, and -/_ to move it to the left.
       Press enter to confirm

    2) Based on the position of the second image, adjust its position to suit you.
       This value will be the x spacing

    Use q to exit the program, and press enter to go to the next assistant
    """
    return XPadding(thumbnail_size).start()

def ypadding_assistant(thumbnail_size):
    """=== Image y spacing ===
    1) Move the second image so that it is just to the bottom of the first image
       Use +/= to move it downwards, and -/_ to move it upwards.
       Press enter to confirm

    2) Based on the current height of the second image, adjust its height to suit you.
       This value will be the y spacing

    Use q to exit the program, and press enter to go to the next assistant
    """
    return YPadding(thumbnail_size).start()


class AbstractImageAdjuster(ABC):
    def __init__(self):
        # Defined in child classes
        self.thumbnail_size: int
        self.show_func: 'func'
        self.side_label: str
        self.start_spaces: int
        self.image: 'pixcat.Image'

        # Defined in start()
        self.width_or_height: int
        self.spaces: int
        self.valid: bool

    @abstractmethod
    def write(self):
        raise NotImplementedError

    @abstractmethod
    def maybe_move_up(self):
        raise NotImplementedError

    @abstractmethod
    def maybe_move_down(self):
        raise NotImplementedError

    @abstractmethod
    def show_func_args(self):
        raise NotImplementedError

    @abstractmethod
    def maybe_erase(self):
        raise NotImplementedError

    @abstractmethod
    def return_tup(self):
        raise NotImplementedError

    @abstractmethod
    def is_input_valid(self):
        raise NotImplementedError

    def hide_show_print(self):
        hide_if_exist(self.image)

        self.image = self.show_func_args()

        self.maybe_move_up()
        write('\r' + ' ' * 20 + '\r')
        self.write()

    def start(self):
        self.maybe_move_down()

        self.spaces = self.start_spaces
        self.valid = True

        with term.cbreak():
            while True:
                if self.is_input_valid():
                    self.hide_show_print()

                ans = term.inkey()
                check_quit(ans)

                if ans.code == ENTER and self.image:
                    self.maybe_erase()
                    return self.return_tup()

                if ans in PLUS:
                    self.spaces += 1
                    self.valid = True

                elif ans in MINUS and self.spaces > self.start_spaces:
                    self.spaces -= 1
                    self.valid = True

                else:
                    self.valid = False


class AbstractPadding(AbstractImageAdjuster, ABC):
    def __init__(self):
        # Base
        self.start_spaces = 0

        # New attributes
        self.doc: str
        self.default_x: int
        self.find_dim_func: 'func'

    def maybe_erase(self):
        return True

    def return_tup(self):
        return self.spaces, self.width_or_height

    def is_input_valid(self) -> bool:
        return bool(self.valid)

    def start(self):
        print_doc(self.doc)

        show_single_x(self.default_x, self.thumbnail_size)

        self.width_or_height, self.image = self.find_dim_func(
            self.thumbnail_size,
        ).start()

        return super().start()

class XPadding(AbstractPadding):
    def __init__(self, thumbnail_size):
        super().__init__()
        # Base
        self.thumbnail_size = thumbnail_size
        self.show_func = show_single_x
        self.side_label = 'width'

        # Padding ABC
        self.doc = xpadding_assistant.__doc__
        self.default_x = config.xcoords_config()[0]
        self.find_dim_func = FindImageWidth

    def write(self):
        write(f'x spacing = {self.spaces}')

    def maybe_move_down(self, *a):
        return True

    def maybe_move_up(self):
        return True

    def show_func_args(self):
        return self.show_func(
            self.default_x + self.width_or_height + self.spaces,
            self.thumbnail_size
        )


class YPadding(AbstractPadding):
    def __init__(self, thumbnail_size):
        super().__init__()
        # Base
        self.thumbnail_size = thumbnail_size
        self.show_func = show_single_y
        self.side_label = 'height'

        # Padding ABC
        self.doc = ypadding_assistant.__doc__
        self.default_x = config.xcoords_config()[1]
        self.find_dim_func = FindImageHeight

    def write(self):
        write(f'y spacing = {self.spaces}')

    def maybe_move_down(self):
        move_cursor_down(self.width_or_height)

    def maybe_move_up(self):
        move_cursor_up(self.spaces)

    def show_func_args(self):
        return self.show_func(
            self.width_or_height + self.spaces,
            self.thumbnail_size
        )


class FindImageDimension(AbstractImageAdjuster, ABC):
    def __init__(self):
        # Base
        # Defined in child classes
        self.thumbnail_size: int
        self.show_func: 'func'
        self.side_label: str
        self.start_spaces: int
        self.image = None

    def write(self):
        write(f'image {self.side_label} = {self.spaces - self.start_spaces}')

    def maybe_move_down(self):
        return True

    def show_func_args(self):
        return self.show_func(self.spaces, self.thumbnail_size)

    def maybe_erase(self):
        erase_line()

    def return_tup(self):
        return self.spaces - self.start_spaces, self.image

    def is_input_valid(self):
        return self.spaces >= self.start_spaces and self.valid


class FindImageWidth(FindImageDimension):
    def __init__(self, thumbnail_size):
        self.thumbnail_size = thumbnail_size
        self.show_func = show_single_x
        self.side_label = 'width'
        self.start_spaces = config.xcoords_config()[0]
        super().__init__()

    def maybe_move_up(self):
        return True


class FindImageHeight(FindImageDimension):
    def __init__(self, thumbnail_size):
        self.thumbnail_size = thumbnail_size
        self.show_func = show_single_y
        self.side_label = 'height'
        self.start_spaces = 0
        super().__init__()

    def maybe_move_up(self):
        move_cursor_up(self.spaces)



def page_spacing_assistant(thumbnail_size):
    # This doesn't use print_doc() as a clean state is needed
    os.system('clear')
    print('=== Page spacing ===')
    print('This will display an image, then print newlines.')
    print('Your desired setting is the number when '
          'the image completely scrolls out of view')

    input('\nEnter any key to continue\n')
    os.system('clear')

    copy(SAMPLE_IMAGE).thumbnail(thumbnail_size).show(align='left')

    time.sleep(0.5)

    for i in range(term.height + 5):
        print(i)
        time.sleep(0.1)

    print('When the image just completely scrolls out of view, '
          'what is the largest number?')
    print('(By default on kitty, ctrl+shift+up/down '
          'scrolls up/down a line)')
    return input()


def gallery_print_spacing_assistant(size, image_width, xpadding):
    """=== Gallery print spacing ===
    Use +/= to increase the spacing, and -/_ to decrease it
    Use q to exit the program, and press enter to go to the next assistant
    Use left and right arrow keys to change the current space selection

    Do you want to preview an existing cache dir? [y/N]
    To keep your chosen thumbnail size, image width and x spacing, enter 'n'.
    """
    print_doc(gallery_print_spacing_assistant.__doc__)
    ans = input()

    if ans == 'y':
        path = pick_dir()
        data = FakeData(path)
        lscat.show_instant(lscat.TrackDownloads, data)
        ncols = config.ncols_config()  # Default fallback, on user choice
    else:
        show_instant_sample(size, image_width, xpadding)
        ncols = pure.ncols(term.width, image_width, xpadding)

    print('\n')

    # Just the default settings; len(first_list) == 5
    spacings = [9, 17, 17, 17, 17] + [17] * (ncols - 5)
    current_selection = 0

    with term.cbreak():
        while True:
            move_cursor_up(2)
            erase_line()
            print_cols(spacings, ncols)
            print('\n\nAdjusting the number of spaces between '
                  f'{current_selection} and {current_selection+1}',
                  flush=True)
            move_cursor_up(1)

            ans = term.inkey()
            check_quit(ans)

            if ans in PLUS and line_width(spacings, ncols) < term.width:
                spacings[current_selection] += 1

            elif ans in MINUS and spacings[current_selection] > 0:
                spacings[current_selection] -= 1

            # right arrow
            elif (ans.code == 261 or ans in {'d', 'l'} and
                    current_selection < len(spacings)):
                current_selection += 1

            # left arrow
            elif (ans.code == 260 or ans in {'a', 'h'} and
                    current_selection > 0):
                    current_selection -= 1

            elif ans.code == ENTER:
                return spacings


def user_info_assistant(thumbnail_size, xpadding, image_width):
    """=== User print name xcoord ===
    Use +/= to move the text right, and -/_ to move it left
    Adjust the position as you see fit

    Use q to exit the program, and press enter to confirm the current position
    """
    print_doc(user_info_assistant.__doc__)

    spacing, _ = config.get_gen_users_settings()  # Default
    preview_xcoords = pure.xcoords(term.width, image_width, xpadding, 1)[-3:]

    display_user_row(thumbnail_size, preview_xcoords, xpadding)
    move_cursor_up(5)

    with term.cbreak():
        while True:
            update_user_info(spacing)

            ans = term.inkey()
            check_quit(ans)

            if ans in PLUS:
                spacing += 1

            elif ans in MINUS and spacing > 0:
                spacing -= 1

            elif ans.code == ENTER:
                print('\n' * 3)
                return spacing


def update_user_info(spacing):
    erase_line()         # Erase the first line
    move_cursor_down()   # Go down and erase the second line
    erase_line()
    move_cursor_up(1)    # Go back up to the original position
    print_info(spacing)  # Print info takes up 2 lines
    move_cursor_up(2)    # so go back to the top
