import os
from getpass import getpass
from pathlib import Path
from configparser import ConfigParser

from blessed import Terminal
from returns.result import safe
from placeholder import m

from koneko import lscat

TERM = Terminal()


@safe
def get_config_section(section: str) -> 'Result[config]':
    config_object = ConfigParser()
    config_path = Path('~/.config/koneko/config.ini').expanduser()
    config_object.read(config_path)
    section = config_object[section]
    return section

def get_settings(section: str, setting: str) -> 'Result[str]':
    cfgsection: 'Result[config]' = get_config_section(section)
    return cfgsection.map(m.get(setting, ''))

@safe
def _check_print_info() -> 'Result[bool]':
    """Returns either Success(True), Success(False) or Failure.
    Inner boolean represents whether to print columns or not
    Failure represents no key/setting/config found
    """
    section = get_config_section('misc')
    return section.map(m.getboolean('print_info', fallback=True)).value_or(True)

def check_print_info() -> 'bool':
    """For a Failure (setting not found), return True by default"""
    return _check_print_info().value_or(True)

def _width_paddingx() -> int:
    settings = get_config_section('lscat')
    return (
        settings.map(m.getint('image_width', fallback=18)).value_or(18),
        settings.map(m.getint('images_x_spacing', fallback=2)).value_or(2)
    )

def ncols_config():
    return lscat.ncols(TERM.width, *_width_paddingx())

def xcoords_config(offset=0):
    return lscat.xcoords(TERM.width, *_width_paddingx(), offset)

def ycoords_config():
    settings = get_config_section('lscat')
    img_height = settings.map(m.getint('image_height', fallback=8)).value_or(8)
    paddingy = settings.map(m.getint('images_y_spacing', fallback=1)).value_or(1)
    return lscat.ycoords(TERM.height, img_height, paddingy)

def gallery_page_spacing_config():
    settings = get_config_section('lscat')
    return settings.map(m.getint('gallery_page_spacing', fallback=23)).value_or(23)

def users_page_spacing_config():
    settings = get_config_section('lscat')
    return settings.map(m.getint('users_page_spacing', fallback=20)).value_or(20)

def thumbnail_size_config():
    settings = get_config_section('lscat')
    return settings.map(m.getint('image_thumbnail_size', fallback=310)).value_or(310)

def get_gen_users_settings():
    settings = get_config_section('lscat')
    return (
        settings.map(m.getint('users_print_name_xcoord', fallback=18)).value_or(18),
        settings.map(m.getint('images_x_spacing', fallback=2)).value_or(2)
    )


def credentials_from_config(config_object, config_path):
    config_object.read(config_path)
    credentials = config_object['Credentials']
    your_id = credentials.get('ID', '')
    return credentials, your_id

def begin_config() -> ('config', str):
    config_path = Path('~/.config/koneko/config.ini').expanduser()
    config_object = ConfigParser()
    if config_path.exists():
        return credentials_from_config(config_object, config_path)
    return init_config(config_object, config_path)


def init_config(config_object, config_path):
    config_object = _ask_credentials(config_object)
    config_object, your_id = _ask_your_id(config_object)
    _write_config(config_object, config_path)
    _append_default_config(config_path)
    return config_object['Credentials'], your_id

def _ask_credentials(config_object):
    username = input('Please enter your username:\n')
    print('\nPlease enter your password:')
    password = getpass()
    config_object['Credentials'] = {'Username': username, 'Password': password}
    return config_object

def _ask_your_id(config_object):
    print('\nDo you want to save your pixiv ID? It will be more convenient')
    print('to view artists you are following')
    ans = input()
    if ans == 'y' or not ans:
        your_id = input('Please enter your pixiv ID:\n')
        config_object['Credentials'].update({'ID': your_id})
        return config_object, your_id
    return config_object, ''

def _write_config(config_object, config_path):
    os.system('clear')
    config_path.parent.mkdir(exist_ok=True)
    config_path.touch()
    with open(config_path, 'w') as c:
        config_object.write(c)

def _append_default_config(config_path):
    # Why not use python? Because it's functional, readable, and
    # this one liner defeats any potential speed benefits
    example_cfg = Path('~/.local/share/koneko/example_config.ini').expanduser()
    os.system(f'tail {example_cfg} -n +9 >> {config_path}')
