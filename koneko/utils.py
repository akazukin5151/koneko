"""Small functions with side effects, arranged by functionality.
If one functionality gets too long, move them into their own module.
Functionalities:
    - History and logging
    - pick module
    - Wrapping other functions
    - Calculations
    - File related
    - Print related
    - IO related
    - Interacting with user (should be in frontend!)
    - lscat related
"""

import os
import sys
import imghdr
import logging
import itertools
import threading
from copy import copy
from math import ceil
from pathlib import Path
from shutil import rmtree
from collections import Counter
from contextlib import contextmanager
from logging.handlers import RotatingFileHandler

import funcy
from pick import Picker
from pixcat import Image
from placeholder import m
from blessed import Terminal

from koneko import KONEKODIR, pure
from koneko import colors as c
from koneko.config import ncols_config, xcoords_config


term = Terminal()

# History and logging
def setup_history_log():
    logger = logging.getLogger('history')
    handler = RotatingFileHandler(KONEKODIR / 'history', maxBytes=1e6, backupCount=3)
    formatter = logging.Formatter('%(message)s')

    logger.setLevel(logging.INFO)  # Global, applies to all handlers
    handler.setFormatter(formatter)
    logger.addHandler(handler)
    return logger

def read_history() -> 'list[str]':
    with cd(KONEKODIR):
        with open('history', 'r') as f:
            history = f.read()

    return history.split('\n')[:-1]  # Ignore trailing \n

def frequent_history(n=5) -> 'dict[str, int]':
    return dict(Counter(read_history()).most_common(n))

def frequent_history_modes(modes: 'list[str]', n=5) -> 'dict[str, int]':
    items_in_mode = filter(
        lambda x: x.split(': ')[0] in modes,
        read_history()
    )
    return dict(Counter(items_in_mode).most_common(n))

def format_frequent(counter: 'dict[str, int]') -> 'list[str]':
    return [f'{k} ({v})' for k,v in counter.items()]


# pick module
def ws_picker(actions, title, **kwargs):
    picker = Picker(actions, title, **kwargs)
    picker.register_custom_handler(ord('w'), m.move_up())
    picker.register_custom_handler(ord('s'), m.move_down())
    return picker

def select_modes_filter(more=False):
    title = "Use SPACE to select a mode to show and ENTER to confirm"
    # Copied from screens
    actions = [
        '1. View artist illustrations',
        '2. Open pixiv post',
        '3. View following artists',
        '4. Search for artists',
    ]

    if more:
        actions.extend(('5. View illustrations of all following artists',
                        'c. Clear all filters'))

    picker = ws_picker(actions, title, multiselect=True, min_selection_count=1)
    selected = picker.start()
    return [str(x[1] + 1) for x in selected]


# Wrapping other functions
@contextmanager
def cd(newdir: Path) -> 'IO':
    """Change current script directory, do something, change back to old directory
    See https://stackoverflow.com/questions/431684/how-do-i-change-the-working-directory-in-python/24176022#24176022

    Parameters
    ----------
    newdir : str
        New directory to cd into inside 'with'
    """
    prevdir = os.getcwd()
    os.chdir(os.path.expanduser(newdir))
    try:
        yield
    finally:
        os.chdir(prevdir)

def _spin(done: 'Event', message: str) -> None:
    for char in itertools.cycle('|/-\\'):  # Infinite loop
        print(message, char, flush=True, end='\r')
        if done.wait(0.1):
            break
    print(' ' * len(char), end='\r')  # clears the spinner

@funcy.decorator
def spinner(call: 'func[T]', message='') -> 'T':
    """See http://hackflow.com/blog/2013/11/03/painless-decorators/"""
    done = threading.Event()
    spinner_thread = threading.Thread(target=_spin, args=(done, message))
    spinner_thread.start()
    try:
        return call()  # Run the wrapped function
    finally:
        # On exception, stop the spinner
        done.set()
        spinner_thread.join()

@funcy.decorator
def catch_ctrl_c(call: 'func[T]') -> 'T':
    """See http://hackflow.com/blog/2013/11/03/painless-decorators/"""
    try:
        return call()
    except KeyboardInterrupt:
        os.system('clear')


# Calculations
def seq_coords_to_int(keyseqs: 'list[str]') -> 'Optional[int]':
    """Takes prompt input key seqs, find the selected image number.
    If find_number_map() returns None, prompt.goto_image() will catch it.
    """
    first_num, second_num = keyseqs[-2:]
    return find_number_map(int(first_num), int(second_num))

def find_number_map(x: int, y: int) -> 'Optional[int]':
    """Translates 1-based-index coordinates into (0-) indexable number
    For 5 cols and 6 rows:
        5 columns and 6 rows == 30 images
        -1 accounts for the input being 1-based-index but python being 0-based
        mod 5: x is cyclic for every 5
        +5y: adding a 5 for every row 'moves one square down' on the 5x6 grid

    >>> a = [find_number_map(x,y) for y in range(1,7) for x in range(1,6)]
    >>> assert a == list(range(30))
    """
    ncols = ncols_config()
    nrows = ceil(30 / ncols)
    if 1 <= x <= ncols and 1 <= y <= nrows:
        return ((x - 1) % ncols) + (ncols * (y - 1))


# File related
def remove_dir_if_exist(data):
    if data.download_path.is_dir():
        rmtree(data.download_path)

def verify_full_download(filepath: Path) -> bool:
    verified = imghdr.what(filepath)
    if not verified:
        os.remove(filepath)
        return False
    return True

def dir_up_to_date(data, _dir) -> bool:
    # O(1) time
    if len(_dir) < len(data.all_names):
        return False

    # Should not fail because try-except early returned
    for name, _file in zip(data.all_names, sorted(_dir)):
        if name.replace('/', '') not in _file:
            return False
    return True

def dir_not_empty(data: 'Data') -> bool:
    if data.download_path.is_dir() and (_dir := os.listdir(data.download_path)):

        # Is a valid directory and it's not empty, but data has not been fetched yet
        try:
            data.all_names
        except (KeyError, AttributeError):
            return True

        # Exclude the .koneko file
        if '.koneko' in sorted(_dir)[0]:
            return dir_up_to_date(data, sorted(_dir)[1:])

        return dir_up_to_date(data, _dir)

    return False


# Print related
def write(value: str) -> 'IO':
    print(value, end='', flush=True)

def move_cursor_up(num: int) -> 'IO':
    if num > 0:
        write(f'\033[{num}A')

def move_cursor_down(num=1) -> 'IO':
    if num > 0:
        write(f'\033[{num}B')

def erase_line() -> 'IO':
    write('\033[K')

def print_cols(spacings: 'list[int]', ncols: int) -> 'IO':
    for (idx, space) in enumerate(spacings[:ncols]):
        write(' ' * int(space))
        write(idx + 1)

def print_info(message_xcoord: int) -> 'IO':
    print(' ' * message_xcoord, '000', '\n',
          ' ' * message_xcoord, 'Example artist', sep='')

def print_doc(doc: str) -> 'IO':
    """Prints a given string in the bottom of the terminal"""
    os.system('clear')
    number_of_newlines = doc.count('\n')
    bottom = term.height - (number_of_newlines + 2)
    move_cursor_down(bottom)
    print(doc)

def print_multiple_imgs(illusts_json: 'Json') -> None:
    HASHTAG = f'{c.RED}#'
    HAS = f'{c.RESET} has {c.BLUE}'
    OF_PAGES = f'{c.RESET} pages'
    _ = [print(f'{HASHTAG}{index}{HAS}{number}{OF_PAGES}', end=', ')
         for (index, _json) in enumerate(illusts_json)
         if (number := _json['page_count']) > 1]
    print('')

def update_gallery_info(spacings, ncols, current_selection):
    move_cursor_up(2)
    erase_line()
    print_cols(spacings, ncols)
    print('\n\nAdjusting the number of spaces between '
          f'{current_selection} and {current_selection+1}',
          flush=True)
    move_cursor_up(1)

def update_user_info(spacing):
    erase_line()         # Erase the first line
    move_cursor_down()   # Go down and erase the second line
    erase_line()
    move_cursor_up(1)    # Go back up to the original position
    print_info(spacing)  # Print info takes up 2 lines
    move_cursor_up(2)    # so go back to the top


# IO related
def open_in_browser(image_id) -> 'IO':
    link = f'https://www.pixiv.net/artworks/{image_id}'
    os.system(f'xdg-open {link}')
    print(f'Opened {link} in browser!')

def open_link_coords(data, first_num, second_num) -> 'IO':
    selected_image_num = find_number_map(int(first_num), int(second_num))
    # 0 is acceptable; 0 is falsy but not False
    if selected_image_num is False:
        print('Invalid number!')
    else:
        open_link_num(data, selected_image_num)

def open_link_num(data, number) -> 'IO':
    # Update current_page_illusts, in case if you're in another page
    open_in_browser(data.image_id(number))

def handle_missing_pics() -> 'IO':
    basedir = Path('~/.local/share/koneko/pics').expanduser()
    if basedir.exists():
        return True

    print('Please wait, downloading welcome image (this will only occur once)...')
    baseurl = 'https://raw.githubusercontent.com/twenty5151/koneko/master/pics/'

    basedir.mkdir(parents=True)
    for pic in ('71471144_p0.png', '79494300_p0.png'):
        os.system(f'curl -s {baseurl}{pic} -o {basedir}/{pic}')

    os.system('clear')


# Interacting with user (should be in frontend!)
def check_quit(ans: str):
    if ans == 'q':
        sys.exit(0)

def ask_your_id(your_id):
    if your_id:  # your_id stored in config file
        ans = input('Do you want to use the Pixiv ID saved in your config? [Y/n]\n')
        if ans in {'y', ''}:
            return your_id

    # If your_id not stored, or if ans is no, or if id provided, via cli
    return ''



# Lscat related
def show_single(x: int, y: int, thumbnail_size: int) -> 'IO[Image]':
    # Must make a copy before using this reference
    SAMPLE_IMAGE = Image(KONEKODIR.parent / 'pics' / '71471144_p0.png')
    img = copy(SAMPLE_IMAGE).thumbnail(thumbnail_size)
    img.show(align='left', x=x, y=y)
    return img

def show_single_x(x: int, thumbnail_size: int) -> 'IO[Image]':
    return show_single(x, 0, thumbnail_size)

def show_single_y(y: int, thumbnail_size: int) -> 'IO[Image]':
    # Default usage of config module
    return show_single(xcoords_config()[1], y, thumbnail_size)

def show_instant_sample(thumbnail_size, xpadding, image_width: int) -> 'IO':
    xcoords = pure.xcoords(term.width, image_width, xpadding)
    for x in xcoords:
        show_single(x, 0, thumbnail_size)

def display_user_row(size, padding: int, preview_xcoords: 'list[int]') -> 'IO':
    show_single(padding, 0, size)
    for px in preview_xcoords:
        show_single_x(px, size)

def hide_if_exist(image: Image) -> 'IO':
    if image:
        image.hide()
        move_cursor_up(1)

