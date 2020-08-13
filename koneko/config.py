"""Functions to read and write user configuration.

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


def parse_bool(string):
    return string.lower() in {'true', 'yes', 'on', '1'}

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
    def get_setting(self, section, setting) -> 'Result[str]':
        return self.config[section][setting]

    @safe
    def credentials(self) -> 'Result[dict[str, str]]':
        return self.config['Credentials']

    def use_ueberzug(self) -> bool:
        return (self.get_setting('experimental', 'use_ueberzug')
                .map(parse_bool)
                .value_or(True))

    def scroll_display(self) -> bool:
        return (self.get_setting('experimental', 'scroll_display')
                .map(parse_bool)
                .value_or(True))

    def check_image_preview(self) -> bool:
        return (self.get_setting('experimental', 'image_mode_previews')
                .map(parse_bool)
                .value_or(False))

    def check_print_info(self) -> bool:
        return (self.get_setting('misc', 'print_info')
                .map(parse_bool)
                .value_or(True))

    def gallery_page_spacing_config(self) -> int:
        return self.get_setting('lscat', 'page_spacing').map(int).value_or(23)

    def users_page_spacing_config(self) -> int:
        return self.gallery_page_spacing_config() - 3

    def thumbnail_size_config(self) -> int:
        return self.get_setting('lscat', 'image_thumbnail_size').map(int).value_or(310)

    def ueberzug_center_spaces(self) -> int:
        return (self.get_setting('experimental', 'ueberzug_center_spaces')
                .map(int)
                .value_or(20))

    def get_gen_users_settings(self) -> 'tuple[int, int]':
        return (
            self.get_setting('lscat', 'users_print_name_xcoord').map(int).value_or(18),
            self.get_setting('lscat', 'images_x_spacing').map(int).value_or(2),
        )

    def gallery_print_spacing_config(self) -> 'list[int]':
        return (
            self.get_setting('lscat', 'gallery_print_spacing')
            .map(m.split(','))
            .value_or(['9', '17', '17', '17', '17'])
        )

    def dimension(self, side, dimension, fallbacks) -> 'tuple[int, int]':
        return (
            self.get_setting('lscat', f'image_{side}')
                .map(int)
                .value_or(fallbacks[0]),
            self.get_setting('lscat', f'images_{dimension}_spacing')
                .map(int)
                .value_or(fallbacks[1]),
        )


api = Config(Path('~/.config/koneko/config.ini').expanduser())


# While not pure (as reading config is IO), they will always return a default on fail
def ncols_config() -> int:
    return pure.ncols(TERM.width, *api.dimension('width', 'x', (18, 2)))


def nrows_config() -> int:
    return pure.nrows(TERM.height, *api.dimension('height', 'y', (8, 1)))


def xcoords_config(offset=0) -> 'list[int]':
    return pure.xcoords(TERM.width, *api.dimension('width', 'x', (18, 2)), offset)


def ycoords_config() -> 'list[int]':
    return pure.ycoords(TERM.height, *api.dimension('height', 'y', (8, 1)))


# Technically frontend
def begin_config() -> 'tuple[dict[str, str], str]':
    os.system('clear')
    config_path = Path('~/.config/koneko/config.ini').expanduser()
    if config_path.exists():
        return api.credentials().unwrap(), api.get_setting('Credentials', 'id').unwrap()
    return init_config(config_path)


def init_config(config_path) -> 'tuple[dict[str, str], str]':
    credentials = _ask_credentials()
    credentials, your_id = _ask_your_id(credentials)

    _write_config(credentials, config_path)
    _append_default_config(config_path)
    return credentials, your_id


def _ask_credentials() -> 'dict[str, str]':
    return {
        'username': input('Please enter your username:\n'),
        'password': getpass('\nPlease enter your password: ')
    }


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
