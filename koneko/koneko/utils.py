import os
import imghdr
import shutil
from getpass import getpass
from pathlib import Path
from subprocess import check_output
from configparser import ConfigParser

import pixcat

from koneko import KONEKODIR, ui, cli, pure, __version__


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


# - Prompt functions
def begin_prompt(printmessage=True):
    messages = (
        '',
        f'Welcome to koneko v{__version__}\n',
        'Select an action:',
        '1. View artist illustrations',
        '2. Open pixiv post',
        '3. View following artists',
        '4. Search for artists',
        '5. View illustrations of all following artists\n',
        '?. Info',
        'm. Manual',
        'c. Clear koneko cache',
        'q. Quit',
    )
    if printmessage:
        for message in messages:
            print(' ' * 27, message)

    pixcat.Image(
        KONEKODIR.parent / 'pics' / '71471144_p0.png'
    ).thumbnail(550).show(
        align='left', y=0
    )

    # Do it async? But the startup time sleeps to be slow regardless... TODO
    cache_size = check_output(
        f"du -hs --apparent-size {KONEKODIR} | cut -f1",
        shell=True
    ).decode('utf-8').rstrip()
    print(f'Current cache size: {cache_size}')

    command = input('Enter a command: ')
    return command


def artist_user_id_prompt():
    artist_user_id = input('Enter artist ID or url:\n')
    return artist_user_id


@pure.catch_ctrl_c
def show_man_loop():
    os.system('clear')
    print(cli.__doc__)
    print(' ' * 3, '=' * 30)
    print(ui.ArtistGallery.__doc__)
    print(' ' * 3, '=' * 30)
    print(ui.Image.__doc__)
    print(' ' * 3, '=' * 30)
    print(ui.AbstractUsers.__doc__)
    print(' ' * 3, '=' * 30)
    print(ui.IllustFollowGallery.__doc__)
    while True:
        help_command = input('\n\nEnter any key to return: ')
        if help_command or help_command == '':
            os.system('clear')
            break


@pure.catch_ctrl_c
def clear_cache_loop():
    print('Do you want to remove all cached images?')
    print('This will not remove images you explicitly downloaded.')
    while True:
        help_command = input('\nEnter y to confirm: ')
        if help_command == 'y':
            shutil.rmtree('/tmp/koneko/')
            os.system('clear')
            break
        else:
            print('Operation aborted!')
            os.system('clear')
            break


@pure.catch_ctrl_c
def info_screen_loop():
    os.system('clear')
    messages = (
        '',
        f'koneko こねこ version {__version__} beta\n',
        "Browse pixiv in the terminal using kitty's icat to display",
        'images with images embedded in the terminal\n',
        "1. View an artist's illustrations",
        '2. View a post (support multiple images)',
        '3. View artists you followed',
        '4. Search for artists and browse their works.',
        '5. View latest illustrations from artist you follow.\n',
        'Thank you for using koneko!',
        'Please star, report bugs and contribute in:',
        'https://github.com/twenty5151/koneko',
        'GPLv3 licensed\n',
        'Credits to amasyrup (甘城なつき):',
        'Welcome image: https://www.pixiv.net/en/artworks/71471144',
        'Current image: https://www.pixiv.net/en/artworks/79494300',
    )

    for message in messages:
        print(' ' * 26, message)

    pixcat.Image(
        KONEKODIR.parent / 'pics' / '79494300_p0.png'
    ).thumbnail(750).show(
        align='left', y=0
    )

    while True:
        help_command = input('\nEnter any key to return: ')
        if help_command or help_command == '':
            os.system('clear')
            break


def config():
    config_object = ConfigParser()
    if Path('~/.config/koneko/config.ini').expanduser().exists():
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

        config_path = Path('~/.config/koneko/config.ini').expanduser()
        config_path.parent.mkdir(exist_ok=True)
        config_path.touch()
        with open(config_path, 'w') as c:
            config_object.write(c)

        credentials = config_object['Credentials']

        os.system('clear')

    return credentials, your_id

def get_settings(section, setting):
    config_object = ConfigParser()
    config_path = Path('~/.config/koneko/config.ini').expanduser()
    if not config_path.exists():
        return False

    config_object.read(config_path)
    section = config_object[section]
    result = section.get(setting, None)
    return result

def check_noprint():
    noprint = get_settings('misc', 'noprint')
    if noprint == 'on':
        return True
    return False

def noprint(func, *args, **kwargs):
    import contextlib
    with open(os.devnull, "w") as null:
        with contextlib.redirect_stdout(null):
            func(*args, **kwargs)


if __name__ == "__main__":
    noprint(print, "hello")
    print('world')
