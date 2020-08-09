"""Small functions with side effects, arranged by functionality.
If one functionality gets too long, move them into their own module.

Functionalities:
    - History and logging
    - Wrapping other functions
    - Calculations
    - IO related
    - lscat related
"""

import os
import sys
import logging
import itertools
import threading
from math import ceil
from sys import platform
from pathlib import Path
from collections import Counter
from subprocess import check_output
from logging.handlers import RotatingFileHandler

import funcy

from koneko import config, KONEKODIR


# History and logging
def setup_history_log() -> 'logging.Logger':
    logger = logging.getLogger('history')
    handler = RotatingFileHandler(KONEKODIR / 'history', maxBytes=1e6, backupCount=3)
    formatter = logging.Formatter('%(message)s')

    logger.setLevel(logging.INFO)  # Global, applies to all handlers
    handler.setFormatter(formatter)
    logger.addHandler(handler)
    return logger


def read_history() -> 'list[str]':
    with open(KONEKODIR / 'history', 'r') as f:
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
    return [f'{k} ({v})' for (k, v) in counter.items()]


# Wrapping other functions
def _spin(done: 'Event', message: str) -> None:
    for char in itertools.cycle('|/-\\'):  # Infinite loop
        print(message, char, flush=True, end='\r')
        if done.wait(0.1):
            break
    print(' ' * len(char), end='\r')  # clears the spinner


@funcy.decorator
def spinner(func: 'func[T]', message='') -> 'T':
    """See http://hackflow.com/blog/2013/11/03/painless-decorators/"""
    done = threading.Event()
    spinner_thread = threading.Thread(target=_spin, args=(done, message))
    spinner_thread.start()
    try:
        return func()  # Run the wrapped function
    finally:
        # On exception, stop the spinner
        done.set()
        spinner_thread.join()


@funcy.decorator
def catch_ctrl_c(func: 'func[T]') -> 'T':
    """See http://hackflow.com/blog/2013/11/03/painless-decorators/"""
    try:
        return func()
    except KeyboardInterrupt:
        os.system('clear')


# Calculations
def max_images():
    """Calculates the maximum number of images that can fit in single terminal page"""
    return config.ncols_config() * config.nrows_config()


def max_images_user():
    """Calculates the maximum number of images that can fit in single terminal page
    For User Modes, there are 3 previews + 1 artist image, hence always 4 columns
    """
    return 4 * config.nrows_config()


def max_terminal_scrolls(data, is_gallery_mode: bool) -> int:
    number_of_images = len(os.listdir(data.download_path))
    if is_gallery_mode:
        return number_of_images // max_images() + 1
    return number_of_images // max_images_user()


def slice_images(max_images: int, terminal_page: int) -> slice:
    return slice(max_images * terminal_page, max_images * (terminal_page + 1))


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
    ncols = config.ncols_config()
    nrows = ceil(30 / ncols)
    if 1 <= x <= ncols and 1 <= y <= nrows:
        return ((x - 1) % ncols) + (ncols * (y - 1))
    return False


# IO related
def open_in_browser(image_id: str) -> 'IO':
    link = f'https://www.pixiv.net/artworks/{image_id}'
    opener = 'open' if platform == 'darwin' else 'xdg-open'
    os.system(f'{opener} {link}')
    print(f'Opened {link} in browser!')


def open_link_coords(data, first_num, second_num) -> 'IO':
    selected_image_num = find_number_map(int(first_num), int(second_num))
    # 0 is acceptable; 0 is falsy but not False
    if selected_image_num is False:
        print('Invalid number!')
    else:
        open_link_num(data, selected_image_num)


def open_link_num(data, number: int) -> 'IO':
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


def get_cache_size() -> str:
    extra = '' if platform == 'darwin' else '--apparent-size'
    return (
        check_output(f'du -hs {extra} {KONEKODIR} | cut -f1', shell=True)
        .decode('utf-8')
        .rstrip()
    )


def quit_on_q(ans: str):
    if ans == 'q':
        sys.exit(0)


# Ueberzug
def try_import_ueberzug():
    try:
        import ueberzug.lib.v0 as ueberzug
    except ImportError as e:
        raise ImportError("Install with `pip install ueberzug`") from e
    return ueberzug
