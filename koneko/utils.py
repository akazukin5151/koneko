"""Small functions that do IO (file read/write, user input, network request, configs)"""

import os
import imghdr
import logging
import itertools
import threading
from math import ceil
from pathlib import Path
from shutil import rmtree
from collections import Counter
from contextlib import contextmanager
from logging.handlers import RotatingFileHandler

import funcy
from pick import Picker
from placeholder import m

from koneko import KONEKODIR
from koneko.config import ncols_config


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


def ws_picker(actions, title, **kwargs):
    picker = Picker(actions, title, **kwargs)
    picker.register_custom_handler(ord('w'), m.move_up())
    picker.register_custom_handler(ord('s'), m.move_down())
    return picker


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

@funcy.decorator
def catch_ctrl_c(call: 'func[T]') -> 'T':
    """See http://hackflow.com/blog/2013/11/03/painless-decorators/"""
    try:
        return call()
    except KeyboardInterrupt:
        os.system('clear')

# From ui
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

def remove_dir_if_exist(data):
    if data.download_path.is_dir():
        rmtree(data.download_path)
