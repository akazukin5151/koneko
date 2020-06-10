import os
import imghdr
import itertools
import threading
from math import ceil
from getpass import getpass
from pathlib import Path
from contextlib import contextmanager
from configparser import ConfigParser

import funcy

from koneko import lscat


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
    ncols = lscat.ncols_config()
    nrows = ceil(30 / ncols)
    if 1 <= x <= ncols and 1 <= y <= nrows:
        return ((x - 1) % ncols) + (ncols * (y - 1))


@contextmanager
def cd(newdir: Path) -> None:
    """
    Change current script directory, do something, change back to old directory
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
    """
    See http://hackflow.com/blog/2013/11/03/painless-decorators/
    """
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

def dir_not_empty(data: 'Data') -> bool:
    # If it breaks, try len([x for x in os.listdir(path) if os.is_file(x)])
    if data.download_path.is_dir() and (_dir := os.listdir(data.download_path)):
        try:
            first_img = data.first_img
        except (KeyError, AttributeError):
            return True

        if '.koneko' in sorted(_dir)[0] and first_img in sorted(_dir)[1]:
            return True
        if first_img in sorted(_dir)[0]:
            return True

    return False

# From ui
def open_in_browser(image_id):
    link = f'https://www.pixiv.net/artworks/{image_id}'
    os.system(f'xdg-open {link}')
    print(f'Opened {link} in browser!')

def open_link_coords(data, first_num, second_num):
    selected_image_num = find_number_map(int(first_num), int(second_num))
    # 0 is acceptable; 0 is falsy but not False
    if selected_image_num is False:
        print('Invalid number!')
    else:
        open_link_num(data, selected_image_num)

def open_link_num(data, number):
    # Update current_page_illusts, in case if you're in another page
    open_in_browser(data.image_id(number))


# If config functions get longer/more, consider moving them to a seperate module
def config() -> ('config', 'Optional[str]'):
    config_path = Path('~/.config/koneko/config.ini').expanduser()
    config_object = ConfigParser()
    if config_path.exists():
        config_object.read(Path('~/.config/koneko/config.ini').expanduser())
        credentials = config_object['Credentials']
        # If your_id is stored in the config
        your_id = credentials.get('ID', None)
        return credentials, your_id

    username = input('Please enter your username:\n')
    print('\nPlease enter your password:')
    password = getpass()
    config_object['Credentials'] = {'Username': username, 'Password': password}

    print('\nDo you want to save your pixiv ID? It will be more convenient')
    print('to view artists you are following')
    ans = input()
    if ans == 'y' or not ans:
        your_id = input('Please enter your pixiv ID:\n')
        config_object['Credentials'].update({'ID': your_id})
    else:
        your_id = None

    os.system('clear')

    config_path.parent.mkdir(exist_ok=True)
    config_path.touch()
    with open(config_path, 'w') as c:
        config_object.write(c)

    # Append the default settings to the config file
    # Why not use python? Because it's functional, readable, and
    # this one liner defeats any potential speed benefits
    example_cfg = Path("~/.local/share/koneko/example_config.ini").expanduser()
    os.system(f'tail {example_cfg} -n +9 >> {config_path}')

    credentials = config_object['Credentials']

    return credentials, your_id

def get_config_section(section: str) -> 'config':
    config_object = ConfigParser()
    config_path = Path('~/.config/koneko/config.ini').expanduser()
    if not config_path.exists():
        return False

    config_object.read(config_path)
    section = config_object[section]
    return section

def get_settings(section: str, setting: str) -> 'Optional[str]':
    cfgsection = get_config_section(section)
    if not cfgsection:
        return False
    return cfgsection.get(setting, None)

def check_noprint() -> bool:
    section = get_config_section('misc')
    if not section:
        return False
    try:
        return section.getboolean('noprint', fallback=False)
    except ValueError:
        return False

def noprint(func: 'func[T]', *args, **kwargs) -> 'T':
    import contextlib
    with open(os.devnull, "w") as null:
        with contextlib.redirect_stdout(null):
            func(*args, **kwargs)


if __name__ == "__main__":
    noprint(print, "hello")
    print('world')
