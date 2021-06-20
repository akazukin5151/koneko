"""Functions to read and write user configuration.

Structure:
    - Safe config setting getters, will return default on failure
    - Calculations
    - Interactive config functions for first launch setup
"""
import os
from enum import Enum
from pathlib import Path
from getpass import getpass
from configparser import ConfigParser

from placeholder import m
from returns.result import safe

from koneko import pure, TERM


@safe
def parse_bool(string) -> 'Result[bool, ValueError]':
    if string.lower() in {'true', 'yes', 'on', '1'}:
        return True
    elif string.lower() in {'false', 'no', 'off', '0'}:
        return False
    raise ValueError('Not a boolean!')


@safe
def parse_int(string) -> 'Result[int, ValueError]':
    return int(string)


@safe
def parse_str_list(lst: 'list[str]') -> 'Result[list[int], Exception]':
    return list(map(int, lst))


class Dimension(Enum):
    x = 'width'
    y = 'height'


class Config:
    def __init__(self, path):
        self.config_path = path
        config_object = ConfigParser()
        config_object.read(self.config_path)
        self.config: 'dict[str, dict[str, str]' = {
            section: dict(config_object.items(section))
            for section in config_object.sections()
        }

    @safe
    def get_setting(self, section: str, setting: str) -> 'Result[str, KeyError]':
        return self.config[section][setting]

    def _get_bool(self, section: str, setting: str, default: bool) -> bool:
        return self.get_setting(section, setting).bind(parse_bool).value_or(default)

    def _get_int(self, section: str, setting: str, default: int) -> int:
        return self.get_setting(section, setting).bind(parse_int).value_or(default)


    @safe
    def credentials(self) -> 'Result[dict[str, str], KeyError]':
        return self.config['Credentials']

    def use_ueberzug(self) -> bool:
        return self._get_bool('experimental', 'use_ueberzug', False)

    def scroll_display(self) -> bool:
        return self._get_bool('experimental', 'scroll_display', True)

    def image_mode_previews(self) -> bool:
        return self._get_bool('experimental', 'image_mode_previews', False)

    def print_info(self) -> bool:
        return self._get_bool('misc', 'print_info', True)

    def page_spacing(self) -> int:
        return self._get_int('lscat', 'page_spacing', 23)

    def users_page_spacing(self) -> int:
        return self.page_spacing() - 3

    def thumbnail_size(self) -> int:
        return self._get_int('lscat', 'thumbnail_size', 310)

    def ueberzug_center_spaces(self) -> int:
        return self._get_int('experimental', 'ueberzug_center_spaces', 20)

    def gen_users_settings(self) -> 'tuple[int, int]':
        return (
            self._get_int('lscat', 'users_print_name_xcoord', 18),
            self._get_int('lscat', 'images_x_spacing', 2)
        )

    def gallery_print_spacing(self) -> 'list[int]':
        return (
            self.get_setting('lscat', 'gallery_print_spacing')
            .map(m.split(','))
            .bind(parse_str_list)
            .value_or([9, 17, 17, 17, 17])
        )

    def dimension(self, dimension: Dimension, fallbacks) -> 'tuple[int, int]':
        return (
            self._get_int('lscat', f'image_{dimension.value}', fallbacks[0]),
            self._get_int('lscat', f'images_{dimension.name}_spacing', fallbacks[1]),
        )


api = Config(Path('~/.config/koneko/config.ini').expanduser())


def ncols_config() -> int:
    return pure.ncols(TERM.width, *api.dimension(Dimension.x, (18, 2)))


def nrows_config() -> int:
    return pure.nrows(TERM.height, *api.dimension(Dimension.y, (8, 1)))


def xcoords_config(offset=0) -> 'list[int]':
    return pure.xcoords(TERM.width, *api.dimension(Dimension.x, (18, 2)), offset)


def ycoords_config() -> 'list[int]':
    return pure.ycoords(TERM.height, *api.dimension(Dimension.y, (8, 1)))


# Technically frontend
def begin_config() -> 'tuple[dict[str, str], str]':
    os.system('clear')
    config_path = Path('~/.config/koneko/config.ini').expanduser()
    if config_path.exists():
        # TODO: doesn't this None removes the point of the Result monad?
        return api.credentials().unwrap(), api.get_setting('Credentials', 'id').value_or(None)
    return init_config(config_path)


def init_config(config_path) -> 'tuple[dict[str, str], str]':
    credentials = {
        'refresh_token': input('Please enter your refresh token:\n')
    }
    credentials, your_id = _ask_your_id(credentials)

    _write_config(credentials, config_path)
    _append_default_config(config_path)
    return credentials, your_id


def _ask_your_id(credentials) -> 'tuple[dict[str, str], str]':
    print('\nDo you want to save your pixiv ID? It will be more convenient')
    print('to view artists you are following')
    ans = input()
    if ans == 'y' or not ans:
        your_id = input('Please enter your pixiv ID:\n')
        credentials.update({'ID': your_id})
        return credentials, your_id
    return credentials, ''


def _write_config(credentials, config_path) -> 'IO':
    os.system('clear')
    parser = ConfigParser()
    parser.read_dict({'Credentials': credentials})
    config_path.parent.mkdir(exist_ok=True)
    config_path.touch()
    with open(config_path, 'w') as c:
        parser.write(c)


def _append_default_config(config_path) -> 'IO':
    # Why not use python? Because it's functional, readable, and
    # this one liner defeats any potential speed benefits
    example_cfg = Path('~/.local/share/koneko/example_config.ini').expanduser()
    os.system(f'tail {example_cfg} -n +9 >> {config_path}')
