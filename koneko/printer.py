import os

from koneko import TERM
from koneko import colors as c


def write(value: str) -> 'IO':
    print(value, end='', flush=True)


def move_cursor_up(num: int) -> 'IO':
    if num > 0:
        write(f'\033[{num}A')


def move_cursor_down(num=1) -> 'IO':
    if num > 0:
        write(f'\033[{num}B')


def move_cursor_xy(x, y):
    write(f'\033[{x};{y}H')


def erase_line() -> 'IO':
    write('\033[K')


def print_cols(spacings: 'list[int]', ncols: int) -> 'IO':
    for (idx, space) in enumerate(spacings[:ncols]):
        write(' ' * int(space))
        write(idx + 1)


def _print_info(message_xcoord: int) -> 'IO':
    print(' ' * message_xcoord, '000', '\n',
          ' ' * message_xcoord, 'Example artist', sep='')


def maybe_print_size(actions, size) -> 'IO':
    if 1 in actions or 7 in actions:
        print(f'image_thumbnail_size = {size}')


def maybe_print_width_xpadding(actions, image_width, xpadding) -> 'IO':
    if 2 in actions or 7 in actions:
        print(f'image_width = {image_width}')
        print(f'images_x_spacing = {xpadding}')


def maybe_print_height_ypadding(actions, image_height, ypadding) -> 'IO':
    if 3 in actions or 7 in actions:
        print(f'image_height = {image_height}')
        print(f'images_y_spacing = {ypadding}')


def maybe_print_page_spacing(actions, page_spacing) -> 'IO':
    if 4 in actions or 7 in actions:
        print(f'page_spacing = {page_spacing}')


def maybe_print_print_spacing(actions, gallery_print_spacing) -> 'IO':
    if 5 in actions or 7 in actions:
        print('gallery_print_spacing =',
              ','.join((str(x) for x in gallery_print_spacing)))


def maybe_print_user_info(actions, user_info_xcoord) -> 'IO':
    if 6 in actions or 7 in actions:
        print(f'users_print_name_xcoord = {user_info_xcoord}')


def print_doc(doc: str) -> 'IO':
    """Prints a given string in the bottom of the terminal"""
    os.system('clear')
    number_of_newlines = doc.count('\n')
    bottom = TERM.height - (number_of_newlines + 2)
    move_cursor_down(bottom)
    print(doc)


def print_multiple_imgs(illusts_json: 'Json') -> 'IO':
    HASHTAG = f'{c.RED}#'
    HAS = f'{c.RESET} has {c.BLUE}'
    OF_PAGES = f'{c.RESET} pages'

    for (index, _json) in enumerate(illusts_json):
        if (number := _json['page_count']) > 1:
            print(f'{HASHTAG}{index}{HAS}{number}{OF_PAGES}', end=', ')

    print('')


def update_gallery_info(spacings, ncols, current_selection) -> 'IO':
    move_cursor_up(2)
    erase_line()
    print_cols(spacings, ncols)
    print('\n\nAdjusting the number of spaces between '
          f'{current_selection} and {current_selection+1}',
          flush=True)
    move_cursor_up(1)


def update_user_info(spacing) -> 'IO':
    erase_line()          # Erase the first line
    move_cursor_down()    # Go down and erase the second line
    erase_line()
    move_cursor_up(1)     # Go back up to the original position
    _print_info(spacing)  # Print info takes up 2 lines
    move_cursor_up(2)     # so go back to the top


def image_help() -> 'IO':
    print('')
    print(''.join([
        c.b, 'ack; ',
        c.n, 'ext image; ',
        c.p, 'revious image; ',
        c.d_, 'ownload image;',
        c.o_, 'pen image in browser;\n',
        'show image in', c.f, 'ull res; ',
        c.q, 'uit (with confirmation); ',
        'view ', c.m, 'anual\n'
    ]))


def user_help() -> 'IO':
    print('')
    print(''.join([
        'view ', c.BLUE_N, "th artist's illusts ",
        c.n, 'ext page; ',
        c.p, 'revious page; ',
        c.r, 'eload and re-download all;\n',
        c.q, 'uit (with confirmation);',
        'view ', c.m, 'anual\n'
    ]))
