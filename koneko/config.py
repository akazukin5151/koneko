"""Functions to read and write user configuration.
The IOResult type is just to mark functions using IO;
they don't return an IOResult container

Structure:
    - Impure config file IO
    - Safe config setting getters, will return default on failure
    - Interactive config functions for first launch setup
"""

import os
from pathlib import Path
from getpass import getpass
from configparser import ConfigParser

from placeholder import m
from returns.result import safe
from returns.pipeline import flow

from koneko import pure, TERM


# Impure
@safe
def get_config_section(section: str) -> 'IOResult[config]':
    config_object = ConfigParser()
    config_path = Path('~/.config/koneko/config.ini').expanduser()
    config_object.read(config_path)
    section = config_object[section]
    return section


def get_settings(section: str, setting: str) -> 'IOResult[str]':
    cfgsection: 'Result[config]' = get_config_section(section)
    return cfgsection.map(m.get(setting, ''))


@safe
def unsafe_get_bool_config(section, setting: str, fallback: bool) -> 'IOResult[bool]':
    """Returns either Success(True), Success(False) or Failure.
    Inner boolean represents the value
    Failure represents no key/setting/config found
    """
    section = get_config_section(section)
    return section.map(m.getboolean(setting, fallback=fallback)).value_or(fallback)


# While not pure (as reading config is IO), they will always return a default on fail
def get_bool_config(section: str, setting: str, fallback: bool) -> bool:
    """Reads requested section and setting, returning the fallback on failure."""
    return unsafe_get_bool_config(section, setting, fallback).value_or(fallback)


def check_image_preview() -> bool:
    return get_bool_config('experimental', 'image_mode_previews', False)


def check_print_info() -> bool:
    """For a Failure (setting not found), return True by default"""
    return get_bool_config('misc', 'print_info', True)


def _width_padding(side: str, dimension: str, fallbacks: (int, int)) -> (int, int):
    settings = get_config_section('lscat')
    return (
        settings.map(m.getint(f'image_{side}', fallback=fallbacks[0])).value_or(fallbacks[0]),
        settings.map(m.getint(f'images_{dimension}_spacing', fallback=fallbacks[1])).value_or(fallbacks[1])
    )


def ncols_config() -> int:
    return pure.ncols(TERM.width, *_width_padding('width', 'x', (18, 2)))


def nrows_config() -> int:
    return pure.nrows(TERM.height, *_width_padding('height', 'y', (8, 1)))


def xcoords_config(offset=0) -> 'list[int]':
    return pure.xcoords(TERM.width, *_width_padding('width', 'x', (18, 2)), offset)


def ycoords_config() -> 'list[int]':
    return pure.ycoords(TERM.height, *_width_padding('height', 'y', (8, 1)))


def gallery_page_spacing_config() -> int:
    settings = get_config_section('lscat')
    return settings.map(m.getint('page_spacing', fallback=23)).value_or(23)


def users_page_spacing_config() -> int:
    # Because user modes print two lines of info. The other 1 is an offset
    return gallery_page_spacing_config() - 3


def thumbnail_size_config() -> int:
    settings = get_config_section('lscat')
    return settings.map(m.getint('image_thumbnail_size', fallback=310)).value_or(310)


def get_gen_users_settings() -> (int, int):
    settings = get_config_section('lscat')
    return (
        settings.map(m.getint('users_print_name_xcoord', fallback=18)).value_or(18),
        settings.map(m.getint('images_x_spacing', fallback=2)).value_or(2)
    )


def gallery_print_spacing_config() -> 'list[str]':
    return get_settings('lscat', 'gallery_print_spacing').map(
        m.split(',')
    ).value_or(['9', '17', '17', '17', '17'])



def credentials_from_config(config_object, config_path) -> ('config', str):
    credentials = get_config_section('Credentials').unwrap()
    your_id = credentials.get('ID', '')
    return credentials, your_id

# Technically frontend
def begin_config() -> ('IO[config]', str):
    os.system('clear')
    config_path = Path('~/.config/koneko/config.ini').expanduser()
    config_object = ConfigParser()
    if config_path.exists():
        return credentials_from_config(config_object, config_path)
    return init_config(config_object, config_path)


def init_config(config_object, config_path) -> ('IO[config]', str):
    # Identical to `_ask_your_id(_ask_credentials(config_object))`
    config_object, your_id = flow(
        config_object,
        _ask_credentials,
        _ask_your_id
    )

    _write_config(config_object, config_path)
    _append_default_config(config_path)
    return config_object['Credentials'], your_id


def _ask_credentials(config_object) -> 'IO[config]':
    username = input('Please enter your username:\n')
    print('\nPlease enter your password:')
    password = getpass()
    config_object['Credentials'] = {'Username': username, 'Password': password}
    return config_object


def _ask_your_id(config_object) -> ('IO[config]', str):
    print('\nDo you want to save your pixiv ID? It will be more convenient')
    print('to view artists you are following')
    ans = input()
    if ans == 'y' or not ans:
        your_id = input('Please enter your pixiv ID:\n')
        config_object['Credentials'].update({'ID': your_id})
        return config_object, your_id
    return config_object, ''


def _write_config(config_object, config_path) -> 'IO':
    os.system('clear')
    config_path.parent.mkdir(exist_ok=True)
    config_path.touch()
    with open(config_path, 'w') as c:
        config_object.write(c)


def _append_default_config(config_path) -> 'IO':
    # Why not use python? Because it's functional, readable, and
    # this one liner defeats any potential speed benefits
    example_cfg = Path('~/.local/share/koneko/example_config.ini').expanduser()
    os.system(f'tail {example_cfg} -n +9 >> {config_path}')
