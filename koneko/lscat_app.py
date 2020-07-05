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
from pathlib import Path
from shutil import rmtree
from collections import namedtuple

from docopt import docopt

from koneko import utils, files, lscat, config, printer, assistants, KONEKODIR


# Constants
EMPTY_WARNING = "**No directories match the filter! Press 'f' to re-filter**"

# Small 'functions'
FakeData = namedtuple('data', ('download_path',))


def try_filter_dir(modes):
    return sorted(files.filter_dir(modes)) or [EMPTY_WARNING]


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
    actions = files.filter_history(path)
    return pick_dir_loop(path, basetitle, actions, None)


def pick_dir_loop(path, basetitle, actions, modes):
    title = basetitle

    while True:
        picker = utils.pick_dirs_picker(actions, title)
        _, ans = picker.start()
        assistants.check_quit(ans)

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
        actions = files.filter_history(path)
        return basetitle, actions, modes

    title = f"Filtering {modes=}\n" + basetitle
    actions = try_filter_dir(modes)
    return title, actions, modes


def handle_cd(path, actions, ans, modes):
    selected_dir = actions[ans]
    if selected_dir == EMPTY_WARNING:
        return KONEKODIR, None
    elif (newpath := path / selected_dir).is_dir():
        return newpath, modes
    return path, modes


def actions_from_dir(path, modes):
    if path == KONEKODIR and modes is not None:  # Filter active
        return try_filter_dir(modes)
    return files.filter_history(path)


def config_assistance(actions: 'Optional[list[int]]' = None):
    """Some assistants return a new setting, which should be propagated
    to other assistants.
    """
    actions = maybe_ask_assistant(actions)

    size = maybe_thumbnail_size(actions)

    xpadding, image_width = maybe_xpadding_img_width(actions, size)

    ypadding, image_height = maybe_ypadding_img_height(actions, size)

    page_spacing = maybe_page_spacing(actions, size)

    gallery_print_spacing = maybe_print_spacing(actions, size, xpadding, image_width)

    user_info_xcoord = maybe_print_xcoord(actions, size, xpadding, image_width)

    print('\n\nYour recommended settings are:')
    printer.maybe_print_size(actions, size)
    printer.maybe_print_width_xpadding(actions, image_width, xpadding)
    printer.maybe_print_height_ypadding(actions, image_height, ypadding)
    printer.maybe_print_page_spacing(actions, page_spacing)
    printer.maybe_print_print_spacing(actions, gallery_print_spacing)
    printer.maybe_print_user_info(actions, user_info_xcoord)
    input('\nEnter any key to quit\n')


def maybe_ask_assistant(actions):
    if not actions:
        return utils.ask_assistant()
    # Docopt intercepts additional arguments as str
    return [int(x) for x in actions]


def maybe_thumbnail_size(actions):
    if 1 in actions or 7 in actions:
        return assistants.thumbnail_size_assistant()
    return config.thumbnail_size_config()


def maybe_xpadding_img_width(actions, size):
    if 2 in actions or 7 in actions:
        return assistants.xpadding_assistant(size)
    return (
        config.get_gen_users_settings()[1],
        config._width_padding('width', 'x', (0, 2))[0]
    )


def maybe_ypadding_img_height(actions, size):
    if 3 in actions or 7 in actions:
        return assistants.ypadding_assistant(size)
    return None, None


def maybe_page_spacing(actions, size):
    if 4 in actions or 7 in actions:
        return assistants.page_spacing_assistant(size)
    return None, None


def maybe_print_spacing(actions, size, xpadding, image_width):
    if 5 in actions or 7 in actions:
        return assistants.gallery_print_spacing_assistant(
            size, xpadding, image_width
        )


def maybe_print_xcoord(actions, size, xpadding, image_width):
    if 6 in actions or 7 in actions:
        return assistants.user_info_assistant(
            size,
            xpadding,
            image_width
        )

