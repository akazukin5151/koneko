"""Functions to read and write user configuration.

Structure:
    - Safe config setting getters, will return default on failure
    - Calculations
    - Interactive config functions for first launch setup
"""
import os
import sys
from enum import Enum
from pathlib import Path
from getpass import getpass
from configparser import ConfigParser

from placeholder import m
from returns.result import safe
from returns.pipeline import is_successful

from koneko import pure, TERM
from koneko.url_login.open_login_link import open_pixiv_login


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
    # Check if config exists and refresh_token is in the config
    # If yes, proceed normally. Else, launch first_start() and exit
    config_path = Path('~/.config/koneko/config.ini').expanduser()
    if is_successful(api.get_setting('Credentials', 'refresh_token')):
        os.system('clear')
        return (
            api.credentials().unwrap(),
            api.get_setting('Credentials', 'id').value_or('')
        )
    return first_start()


def first_start() -> 'bottom':
    os.system('cp ~/.local/share/koneko/pixiv-url.desktop ~/.local/share/applications')
    os.system('xdg-mime default pixiv-url.desktop x-scheme-handler/pixiv')
    os.system('update-desktop-database ~/.local/share/applications')
    print('Please log to pixiv in your browser then run koneko again')
    login_then_save_verifier()
    sys.exit(0)


def login_then_save_verifier():
    code_verifier = open_pixiv_login()
    path = Path('~/.local/share/koneko/code_verifier').expanduser()
    path.touch(exist_ok=True)
    with open(path, 'w') as f:
        f.write(code_verifier)
