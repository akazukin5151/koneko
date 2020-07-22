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
import logging
import itertools
import threading
from math import ceil
from pathlib import Path
from collections import Counter
from subprocess import check_output
from logging.handlers import RotatingFileHandler

import funcy
from pixcat import Image

from koneko import pure, printer, TERM, KONEKODIR
from koneko.config import ncols_config, xcoords_config


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


def get_cache_size():
    return check_output(
        f'du -hs --apparent-size {KONEKODIR} | cut -f1',
        shell=True
    ).decode('utf-8').rstrip()


# Lscat related
def show_single(x: int, y: int, thumbnail_size: int) -> 'IO[Image]':
    # Must make a copy before using this reference
    img = Image(KONEKODIR.parent / 'pics' / '71471144_p0.png').thumbnail(thumbnail_size)
    img.show(align='left', x=x, y=y)
    return img


def show_single_x(x: int, thumbnail_size: int) -> 'IO[Image]':
    return show_single(x, 0, thumbnail_size)


def show_single_y(y: int, thumbnail_size: int) -> 'IO[Image]':
    # Default usage of config module
    return show_single(xcoords_config()[1], y, thumbnail_size)


def show_instant_sample(thumbnail_size, xpadding, image_width: int) -> 'IO':
    xcoords = pure.xcoords(TERM.width, image_width, xpadding)
    for x in xcoords:
        show_single(x, 0, thumbnail_size)


def display_user_row(size, padding: int, preview_xcoords: 'list[int]') -> 'IO':
    show_single(padding, 0, size)
    for px in preview_xcoords:
        show_single_x(px, size)


def hide_if_exist(image: Image) -> 'IO':
    if image:
        image.hide()
        printer.move_cursor_up(1)

