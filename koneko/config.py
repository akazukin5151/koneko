import os
from getpass import getpass
from pathlib import Path
from configparser import ConfigParser

from blessed import Terminal
from returns.result import safe

from koneko import lscat

TERM = Terminal()


def _width_paddingx() -> int:
    settings = get_config_section('lscat')
    return (
        settings.map(lambda s: s.getint('image_width', fallback=18)).value_or(18),
        settings.map(lambda s: s.getint('images_x_spacing', fallback=2)).value_or(2)
    )

def ncols_config():
    return lscat.ncols(TERM.width, *_width_paddingx())

def xcoords_config(offset=0):
    return lscat.xcoords(TERM.width, *_width_paddingx(), offset)

def ycoords_config():
    settings = get_config_section('lscat')
    img_height = settings.map(
        lambda s: s.getint('image_height', fallback=8)
    ).value_or(8)
    paddingy = settings.map(
        lambda s: s.getint('images_y_spacing', fallback=1)
    ).value_or(1)
    return lscat.ycoords(TERM.height, img_height, paddingy)

def gallery_page_spacing_config():
    settings = get_config_section('lscat')
    return settings.map(
        lambda s: s.getint('gallery_page_spacing', fallback=23)
    ).value_or(23)

def users_page_spacing_config():
    settings = get_config_section('lscat')
    return settings.map(
        lambda s: s.getint('users_page_spacing', fallback=20)
    ).value_or(20)

def thumbnail_size_config():
    settings = get_config_section('lscat')
    return settings.map(
        lambda s: s.getint('image_thumbnail_size', fallback=310)
    ).value_or(310)

def get_gen_users_settings():
    settings = get_config_section('lscat')
    return (
        settings.map(
            lambda s: s.getint('users_print_name_xcoord', fallback=18)
        ).value_or(18),
        settings.map(lambda s: s.getint('images_x_spacing', fallback=2)).value_or(2)
    )


def begin_config() -> ('config', str):
    config_path = Path('~/.config/koneko/config.ini').expanduser()
    config_object = ConfigParser()
    if config_path.exists():
        config_object.read(Path('~/.config/koneko/config.ini').expanduser())
        credentials = config_object['Credentials']
        # If your_id is stored in the config
        your_id = credentials.get('ID', '')
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
        your_id = ''

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

@safe
def get_config_section(section: str) -> 'Result[config]':
    config_object = ConfigParser()
    config_path = Path('~/.config/koneko/config.ini').expanduser()
    config_object.read(config_path)
    section = config_object[section]
    return section

def get_settings(section: str, setting: str) -> 'Result[str]':
    cfgsection: 'Result[config]' = get_config_section(section)
    return cfgsection.map(lambda c: c.get(setting, ''))

@safe
def _check_print_info() -> 'Result[bool]':
    """Returns either Success(True), Success(False) or Failure.
    Inner boolean represents whether to print columns or not
    Failure represents no key/setting/config found
    """
    section = get_config_section('misc')
    return section.map(
        lambda s: s.getboolean('print_info', fallback=True)
    ).value_or(True)

def check_print_info() -> 'bool':
    """For a Failure (setting not found), return True by default"""
    return _check_print_info().value_or(True)
