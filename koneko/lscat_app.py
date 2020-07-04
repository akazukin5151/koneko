"""lscat interactive app

Usage:
  lscat
  lscat (1|c) [<actions> ...]
  lscat (2|g)
  lscat (3|u)
  lscat (4|b)
  lscat (5|p) [<path>]

Optional arguments (for specifying a mode):
  1 c  Koneko configuration assistance
  2 g  Display KONEKODIR / testgallery
  3 u  Display KONEKODIR / testuser
  4 b  Browse a cached dir to display
  5 p  Display a specified path

Possible configuration assistants:
  1  Thumbnail size
  2  x-padding
  3  y-padding
  4  Page spacing
  5  Gallery print spacing
  6  User mode print info x-position
  a  All of the above
"""

import os
import sys
import time
from copy import copy
from pathlib import Path
from shutil import rmtree
from collections import namedtuple
from abc import ABC, abstractmethod

from pixcat import Image
from docopt import docopt

from koneko import pure, utils, lscat, config, TERM, KONEKODIR


# Constants
ENTER = 343
PLUS = {'+', '='}
MINUS = {'-', '_'}
# Must make a copy before using this reference
SAMPLE_IMAGE = Image(KONEKODIR.parent / 'pics' / '71471144_p0.png')
EMPTY_WARNING = "**No directories match the filter! Press 'f' to re-filter**"

# Small 'functions'
FakeData = namedtuple('data', ('download_path',))


def check_quit(ans: str):
    if ans == 'q':
        sys.exit(0)


# Main functions that organise work
def main():
    if len(sys.argv) == 1:
        _main()

    args = docopt(__doc__)

    if args['1'] or args['c']:
        config_assistance(args['<actions>'])

    elif args['2'] or args['g']:
        display_gallery()

    elif args['3'] or args['u']:
        display_user()

    elif args['4'] or args['b']:
        browse_cache()

    elif args['5'] or args['p']:
        display_path(args['<path>'])


def _main():
    os.system('clear')
    title = ('Welcome to the lscat interactive script\n'
             'Please select an action')
    actions = (
        '1. Launch koneko configuration assistance',
        '2. Display KONEKODIR / testgallery',
        '3. Display KONEKODIR / testuser',
        '4. Browse a cached dir to display',
        '5. Display a specified path',
        'Quit'
    )
    picker = utils.ws_picker(actions, title)
    _, ans = picker.start()

    case = {
        0: config_assistance,
        1: display_gallery,
        2: display_user,
        3: browse_cache,
        4: display_path,
    }

    func = case.get(ans, None)
    if func:
        func()


def display_gallery():
    data = FakeData(KONEKODIR / 'testgallery')
    lscat.show_instant(lscat.TrackDownloads, data, True)


def display_user():
    data = FakeData(KONEKODIR / 'testuser')
    lscat.show_instant(lscat.TrackDownloadsUsers, data)


def display_path(path=None):
    if not path:
        path = input('Please paste in your path:\n')
        if not Path(path).is_dir():
            print('Invalid path!')
            sys.exit(1)

    data = FakeData(path)
    lscat.show_instant(lscat.TrackDownloads, data, True)


def browse_cache():
    path = pick_dir()
    data = FakeData(path)

    if '.koneko' in os.listdir(path):
        lscat.show_instant(lscat.TrackDownloadsUsers, data)
    else:
        lscat.show_instant(lscat.TrackDownloads, data, True)


def pick_dir():
    path = KONEKODIR
    # base is immutable
    basetitle = (
        'Select a directory to view\n'
        "Press 'y' to display the current directory\n"
        "Press 'b' to move up a directory\n"
        "Press 'd' to delete the current directory\n"
        "Press 'f' to filter out modes\n"
        "Press 'q' to exit"
    )
    actions = utils.filter_history(path)
    return pick_dir_loop(path, basetitle, actions, None)


def pick_dir_loop(path, basetitle, actions, modes):
    title = basetitle

    while True:
        picker = utils.pick_dirs_picker(actions, title)
        _, ans = picker.start()
        check_quit(ans)

        if ans == 'y':
            return path  # TODO: prevent return if path is not valid

        elif ans == 'b':
            path = handle_back(path)

        elif ans == 'd':
            path = handle_delete(path)

        elif ans == 'f':
            title, actions, modes = handle_filter(path, basetitle)
            continue

        else:
            path, modes = handle_cd(path, actions, ans, modes)

        actions = actions_from_dir(path, modes)


def handle_back(path):
    if path != KONEKODIR:
        return path.parent
    return path


def handle_delete(path):
    print(f'Are you sure you want to delete {path}?')
    confirm = input("Enter 'y' to confirm\n")
    if confirm == 'y':
        rmtree(path)
        return path.parent
    return path


def handle_filter(path, basetitle):
    modes = utils.select_modes_filter(True)
    if '6' in modes:
        # Clear all filters
        actions = utils.filter_history(path)
        return basetitle, actions, modes

    title = f"Filtering {modes=}\n" + basetitle
    actions = sorted(utils.filter_dir(modes)) or [EMPTY_WARNING]
    return title, actions, modes


def handle_cd(path, actions, ans, modes):
    selected_dir = actions[ans]
    if selected_dir == EMPTY_WARNING:
        return path, None
    elif (newpath := path / selected_dir).is_dir():
        return newpath, modes
    return path, modes


def actions_from_dir(path, modes):
    if path == KONEKODIR and modes is not None:  # Filter active
        return sorted(utils.filter_dir(modes))
    return utils.filter_history(path)


def ask_assistant() -> 'IO[list[int]]':
    """Returns a collection of all the indices of actions"""
    title = ('=== Configuration assistance ===\n'
             'Press SPACE to select an action & ENTER to confirm')

    actions = (
        '1. Thumbnail size',
        '2. x-padding',
        '3. y-padding',
        '4. Page spacing',
        '5. Gallery print spacing',
        '6. User mode print info x-position',
        'a. (Run all of the above)\n',
        'Quit'
    )

    picker = utils.ws_picker(actions, title, multiselect=True, min_selection_count=1)
    selected_actions = picker.start()
    return [x[1] + 1 for x in selected_actions]


def config_assistance(actions: 'Optional[list[int]]' = None):
    """Some assistants return a new setting, which should be propagated
    to other assistants.
    """
    if not actions:
        actions = ask_assistant()
    else:
        # Docopt intercepts additional arguments as str
        actions = [int(x) for x in actions]

    if 1 in actions or 7 in actions:
        size = thumbnail_size_assistant()
    else:
        size = config.thumbnail_size_config()  # Fallback

    if 2 in actions or 7 in actions:
        xpadding, image_width = xpadding_assistant(size)
    else:
        # Fallbacks
        _, xpadding = config.get_gen_users_settings()
        image_width, _ = config._width_padding('width', 'x', (0, 2))

    if 3 in actions or 7 in actions:
        ypadding, image_height = ypadding_assistant(size)

    if 4 in actions or 7 in actions:
        page_spacing = page_spacing_assistant(size)

    if 5 in actions or 7 in actions:
        gallery_print_spacing = gallery_print_spacing_assistant(
            size, xpadding, image_width
        )

    if 6 in actions or 7 in actions:
        user_info_xcoord = user_info_assistant(
            size,
            xpadding,
            image_width
        )

    print('\n\nYour recommended settings are:')
    if 1 in actions or 7 in actions:
        print(f'image_thumbnail_size = {size}')

    if 2 in actions or 7 in actions:
        print(f'image_width = {image_width}')
        print(f'images_x_spacing = {xpadding}')

    if 3 in actions or 7 in actions:
        print(f'image_height = {image_height}')
        print(f'images_y_spacing = {ypadding}')

    if 4 in actions or 7 in actions:
        print(f'page_spacing = {page_spacing}')

    if 5 in actions or 7 in actions:
        print('gallery_print_spacing =',
              ','.join((str(x) for x in gallery_print_spacing)))

    if 6 in actions or 7 in actions:
        print(f'users_print_name_xcoord = {user_info_xcoord}')

    input('\nEnter any key to quit\n')


def thumbnail_size_assistant():
    """=== Thumbnail size ===
    This will display an image whose thumbnail size can be varied
    Use +/= to increase the size, and -/_ to decrease it
    Use q to exit the program, and press enter to confirm the size

    Keep in mind this size will be used for a grid of images
    """
    utils.print_doc(thumbnail_size_assistant.__doc__)

    image = copy(SAMPLE_IMAGE)

    size = 300  # starting size
    with TERM.cbreak():
        while True:
            image.thumbnail(size).show(align='left', x=0, y=0)

            ans = TERM.inkey()
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

        # Defined in start()
        self.image: Image
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
        utils.hide_if_exist(self.image)

        self.image = self.show_func_args()

        self.maybe_move_up()
        utils.write('\r' + ' ' * 20 + '\r')
        self.write()

    def start(self):
        self.maybe_move_down()

        self.spaces = self.start_spaces
        self.valid = True

        with TERM.cbreak():
            while True:
                if self.is_input_valid():
                    self.hide_show_print()

                ans = TERM.inkey()
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
        utils.print_doc(self.doc)

        utils.show_single_x(self.default_x, self.thumbnail_size)

        self.width_or_height, self.image = self.find_dim_func(
            self.thumbnail_size,
        ).start()

        return super().start()


class XPadding(AbstractPadding):
    def __init__(self, thumbnail_size):
        super().__init__()
        # Base
        self.thumbnail_size = thumbnail_size
        self.show_func = utils.show_single_x
        self.side_label = 'width'

        # Padding ABC
        self.doc = xpadding_assistant.__doc__
        self.default_x = config.xcoords_config()[0]
        self.find_dim_func = FindImageWidth

    def write(self):
        utils.write(f'x spacing = {self.spaces}')

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
        self.show_func = utils.show_single_y
        self.side_label = 'height'

        # Padding ABC
        self.doc = ypadding_assistant.__doc__
        self.default_x = config.xcoords_config()[1]
        self.find_dim_func = FindImageHeight

    def write(self):
        utils.write(f'y spacing = {self.spaces}')

    def maybe_move_down(self):
        utils.move_cursor_down(self.width_or_height)

    def maybe_move_up(self):
        utils.move_cursor_up(self.spaces)

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
        utils.write(f'image {self.side_label} = {self.spaces - self.start_spaces}')

    def maybe_move_down(self):
        return True

    def show_func_args(self):
        return self.show_func(self.spaces, self.thumbnail_size)

    def maybe_erase(self):
        utils.erase_line()

    def return_tup(self):
        return self.spaces - self.start_spaces, self.image

    def is_input_valid(self):
        return self.spaces >= self.start_spaces and self.valid


class FindImageWidth(FindImageDimension):
    def __init__(self, thumbnail_size):
        self.thumbnail_size = thumbnail_size
        self.show_func = utils.show_single_x
        self.side_label = 'width'
        self.start_spaces = config.xcoords_config()[0]
        super().__init__()

    def maybe_move_up(self):
        return True


class FindImageHeight(FindImageDimension):
    def __init__(self, thumbnail_size):
        self.thumbnail_size = thumbnail_size
        self.show_func = utils.show_single_y
        self.side_label = 'height'
        self.start_spaces = 0
        super().__init__()

    def maybe_move_up(self):
        utils.move_cursor_up(self.spaces)


def page_spacing_assistant(thumbnail_size):
    # This doesn't use print_doc() as a clean state is needed
    os.system('clear')
    print(*(
        '=== Page spacing ===',
        'This will display an image, then print newlines.',
        'Your desired setting is the number when '
        'the image completely scrolls out of view',),
      sep='\n')

    input('\nEnter any key to continue\n')
    os.system('clear')

    copy(SAMPLE_IMAGE).thumbnail(thumbnail_size).show(align='left')

    time.sleep(0.1)

    for i in range(TERM.height + 5):
        print(i)
        time.sleep(0.1)

    print('When the image just completely scrolls out of view, '
          'what is the largest number?')
    print('(By default on kitty, ctrl+shift+up/down '
          'scrolls up/down a line)')

    while True:
        ans = input()
        if ans.isdigit():
            return ans
        print('Must enter a number!')


def gallery_print_spacing_assistant(size, image_width, xpadding):
    """=== Gallery print spacing ===
    Use +/= to increase the spacing, and -/_ to decrease it
    Use q to exit the program, and press enter to go to the next assistant
    Use left and right arrow keys to change the current space selection

    Do you want to preview an existing cache dir? [y/N]
    To keep your chosen thumbnail size, image width and x spacing, enter 'n'.
    """
    utils.print_doc(gallery_print_spacing_assistant.__doc__)  # Action before start
    ans = input()

    # Setup variables
    if ans == 'y':
        _path = pick_dir()
        _data = FakeData(_path)
        lscat.show_instant(lscat.TrackDownloads, _data)
        ncols = config.ncols_config()  # Default fallback, on user choice
    else:
        utils.show_instant_sample(size, image_width, xpadding)
        ncols = pure.ncols(TERM.width, image_width, xpadding)

    # Just the default settings; len(first_list) == 5
    spacings = [9, 17, 17, 17, 17] + [17] * (ncols - 5)
    current_selection = 0

    # Start
    print('\n')
    with TERM.cbreak():
        while True:
            utils.update_gallery_info(spacings, ncols, current_selection)

            ans = TERM.inkey()
            check_quit(ans)

            if ans in PLUS and pure.line_width(spacings, ncols) < TERM.width:
                spacings[current_selection] += 1

            elif ans in MINUS and spacings[current_selection] > 0:
                spacings[current_selection] -= 1

            # right arrow
            elif (ans.code == 261 or ans in {'d', 'l'}
                    and current_selection < len(spacings) - 1):
                current_selection += 1

            # left arrow
            elif (ans.code == 260 or ans in {'a', 'h'}
                    and current_selection > 0):
                current_selection -= 1

            elif ans.code == ENTER:
                return spacings


def user_info_assistant(thumbnail_size, xpadding, image_width):
    """=== User print name xcoord ===
    Use +/= to move the text right, and -/_ to move it left
    Adjust the position as you see fit

    Use q to exit the program, and press enter to confirm the current position
    """
    # Setup variables
    spacing, _ = config.get_gen_users_settings()  # Default
    preview_xcoords = pure.xcoords(TERM.width, image_width, xpadding, 1)[-3:]

    # Start
    utils.print_doc(user_info_assistant.__doc__)

    utils.display_user_row(thumbnail_size, xpadding, preview_xcoords)

    utils.move_cursor_up(5)

    with TERM.cbreak():
        while True:
            utils.update_user_info(spacing)

            ans = TERM.inkey()
            check_quit(ans)

            if ans in PLUS:
                spacing += 1

            elif ans in MINUS and spacing > 0:
                spacing -= 1

            elif ans.code == ENTER:
                print('\n' * 3)
                return spacing


