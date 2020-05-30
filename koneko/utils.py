import os
import imghdr
import itertools
from getpass import getpass
from pathlib import Path
from contextlib import contextmanager
from configparser import ConfigParser

import funcy


@contextmanager
def cd(newdir):
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


def _spin(done, message):
    for char in itertools.cycle('|/-\\'):  # Infinite loop
        print(message, char, flush=True, end='\r')
        if done.wait(0.1):
            break
    print(' ' * len(char), end='\r')  # clears the spinner


@funcy.decorator
def spinner(call, message=''):
    """
    See http://hackflow.com/blog/2013/11/03/painless-decorators/
    """
    done = threading.Event()
    spinner_thread = threading.Thread(target=_spin, args=(done, message))
    spinner_thread.start()
    result = call()  # Run the wrapped function
    done.set()
    spinner_thread.join()
    return result

def verify_full_download(filepath):
    verified = imghdr.what(filepath)
    if not verified:
        os.remove(filepath)
        return False
    return True

def dir_not_empty(data):
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


def config():
    config_path = Path('~/.config/koneko/config.ini').expanduser()
    config_object = ConfigParser()
    if config_path.exists():
        config_object.read(Path('~/.config/koneko/config.ini').expanduser())
        credentials = config_object['Credentials']
        # If your_id is stored in the config
        your_id = credentials.get('ID', None)

    else:
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

def get_config_section(section: str):
    config_object = ConfigParser()
    config_path = Path('~/.config/koneko/config.ini').expanduser()
    if not config_path.exists():
        return False

    config_object.read(config_path)
    section = config_object[section]
    return section

def get_settings(section: str, setting: str):
    cfgsection = get_config_section(section)
    if not cfgsection:
        return False
    return cfgsection.get(setting, None)

def check_noprint():
    section = get_config_section('misc')
    try:
        return section.getboolean('noprint', fallback=False)
    except ValueError:
        return False

def noprint(func, *args, **kwargs):
    import contextlib
    with open(os.devnull, "w") as null:
        with contextlib.redirect_stdout(null):
            func(*args, **kwargs)


if __name__ == "__main__":
    noprint(print, "hello")
    print('world')
