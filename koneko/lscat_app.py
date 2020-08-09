"""lscat interactive app

Usage:
  lscat
  lscat (1|c) [<actions> ...]
  lscat (2|b)
  lscat (3|p) [<path>]
  lscat (4|g)
  lscat (5|u)

Optional arguments (for specifying a mode):
  1 c  Koneko configuration assistance
  2 b  Browse a cached dir to display
  3 p  Display a specified path
  4 g  Display KONEKODIR / testgallery
  5 u  Display KONEKODIR / testuser

Possible configuration assistants:
  1  Thumbnail size
  2  x-padding
  3  y-padding
  4  Page spacing
  5  Gallery print spacing
  6  User mode print info x-position
  7  Ueberzug center images
  8  All of the above
"""

import os
import sys
from pathlib import Path

from docopt import docopt

from koneko import (
    utils,
    lscat,
    config,
    picker,
    printer,
    FakeData,
    KONEKODIR,
    assistants,
    lscat_prompt,
)


# Main functions that organise work
def main() -> 'IO':
    if len(sys.argv) == 1:
        _main()

    args = docopt(__doc__)

    if args['1'] or args['c']:
        config_assistance(args['<actions>'])

    elif args['2'] or args['b']:
        browse_cache()

    elif args['3'] or args['p']:
        display_path(args['<path>'])

    elif args['4'] or args['g']:
        display_gallery()

    elif args['5'] or args['u']:
        display_user()


def _main() -> 'IO':
    ans = picker.lscat_app_main()

    case = {
        0: config_assistance,
        1: browse_cache,
        2: display_path,
        3: display_gallery,
        4: display_user,
    }

    func = case.get(ans, None)
    if func:
        func()


def display_gallery() -> 'IO':
    data = FakeData(KONEKODIR / 'testgallery')
    _display_core(lscat.TrackDownloads, data, utils.max_images())


def display_user() -> 'IO':
    data = FakeData(KONEKODIR / 'testuser')
    _display_core(lscat.TrackDownloadsUsers, data, utils.max_images_user())


def _display_core(tracker, data, max_images):
    if config.use_ueberzug() or not config.scroll_display():
        lscat_prompt.scroll_prompt(tracker, data, max_images)
        input()  # On program exit all images are cleared
    else:
        lscat.show_instant(tracker, data)


def display_path(path=None) -> 'IO':
    if not path:
        path = input('Please paste in your path:\n')

    if not Path(path).is_dir():
        print('Invalid path!')
        sys.exit(1)

    data = FakeData(Path(path))
    _lscat_prompt_switcher(path, data)


def browse_cache() -> 'IO':
    path = picker.pick_dir()
    data = FakeData(path)
    _lscat_prompt_switcher(path, data)


def _lscat_prompt_switcher(path, data):
    if '.koneko' in os.listdir(path):
        lscat_prompt.GalleryUserLoop.for_user(data).start()
    elif 'individual' in str(path):
        lscat_prompt.ImageLoop(path).start()
    else:
        lscat_prompt.GalleryUserLoop.for_gallery(data).start()


def config_assistance(actions: 'Optional[list[int]]' = None) -> 'IO':
    """Some assistants return a new setting, which should be propagated
    to other assistants.
    """
    actions = maybe_ask_assistant(actions)

    size = maybe_thumbnail_size(actions)

    xpadding, image_width = maybe_xpadding_img_width(actions, size)

    ypadding, image_height = maybe_ypadding_img_height(actions, size)

    page_spacing = maybe_page_spacing(actions, size)

    gallery_print_spacing = maybe_print_spacing(
        actions, size, xpadding, image_width, image_height
    )

    user_info_xcoord = maybe_print_xcoord(actions, size, xpadding, image_width)

    ueberzug_center_spaces = maybe_center_spaces(actions)

    print('\n\nYour recommended settings are:')
    printer.maybe_print_size(actions, size)
    printer.maybe_print_width_xpadding(actions, image_width, xpadding)
    printer.maybe_print_height_ypadding(actions, image_height, ypadding)
    printer.maybe_print_page_spacing(actions, page_spacing)
    printer.maybe_print_print_spacing(actions, gallery_print_spacing)
    printer.maybe_print_user_info(actions, user_info_xcoord)
    printer.maybe_print_center_spaces(actions, ueberzug_center_spaces)
    input('\nEnter any key to quit\n')


def maybe_ask_assistant(actions: 'Optional[list[int]]') -> 'list[int]':
    if not actions:
        return picker.ask_assistant()
    # Docopt intercepts additional arguments as str
    return [int(x) for x in actions]


def maybe_thumbnail_size(actions: 'list[int]') -> int:
    if 1 in actions or 8 in actions:
        return assistants.thumbnail_size_assistant()
    return config.thumbnail_size_config()


def maybe_xpadding_img_width(actions: 'list[int]', size: int) -> 'tuple[int]':
    if 2 in actions or 8 in actions:
        return assistants.xpadding_assistant(size)
    return (
        config.get_gen_users_settings()[1],
        config._width_padding('width', 'x', (0, 2))[0],
    )


def maybe_ypadding_img_height(actions: 'list[int]', size: int) -> 'tuple[Optional[int]]':
    if 3 in actions or 8 in actions:
        return assistants.ypadding_assistant(size)
    return None, 8  # Default image height


def maybe_page_spacing(actions: 'list[int]', size: int) -> 'tuple[Optional[int]]':
    if 4 in actions or 8 in actions:
        return assistants.page_spacing_assistant(size)
    return None


def maybe_print_spacing(
    actions: 'list[int]', size, xpadding, image_width, image_height: int
) -> 'list[int]':
    if 5 in actions or 8 in actions:
        return assistants.gallery_print_spacing_assistant(
            size, xpadding, image_width, image_height
        )


def maybe_print_xcoord(actions: 'list[int]', size, xpadding, image_width: int) -> int:
    if 6 in actions or 8 in actions:
        return assistants.user_info_assistant(size, xpadding, image_width)


def maybe_center_spaces(actions: 'list[int]'):
    if 7 in actions or 8 in actions:
        return assistants.center_spaces_assistant()
