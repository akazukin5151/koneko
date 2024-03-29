import configparser
from pathlib import Path
from enum import Enum, auto
from collections import namedtuple
from contextlib import contextmanager

import pytest


@pytest.fixture()
def send_enter(monkeypatch):
    monkeypatch.setattr('builtins.input', lambda *x: '')


@contextmanager
def fakecbreak():
    try:
        yield
    finally:
        pass


@pytest.fixture
def patch_cbreak(monkeypatch):
    monkeypatch.setattr('koneko.TERM.cbreak', fakecbreak)


class CustomExit(SystemExit):
    """Replaces all expected instances of an exit,
    to ensure that code exits only where this exception is mocked into
    """

def raises_customexit(*args, **kwargs):
    """As lambdas don't allow raise statements, this is a function"""
    raise CustomExit()


@pytest.fixture
def use_test_cfg_path(monkeypatch, tmp_path):
    monkeypatch.setattr('koneko.config.Path.expanduser',
                        lambda x: Path(tmp_path / 'test_config.ini'))

class Processer(Enum):
    set = auto()
    delete = auto()

    def __call__(self, section, setting, new_setting=None):
        if self.name == 'set':
            result = namedtuple('action', ('name', 'section', 'setting', 'new_setting'))
            return result(self.name, section, setting, new_setting)
        result = namedtuple('action', ('name', 'section', 'setting'))
        return result(self.name, section, setting)


def setup_test_config(path, Config, *args):
    default = {
        'Credentials': {
            'refresh_token': 'token',
        },
        'lscat': {
            'image_width': 18,
            'image_height': 8,
            'thumbnail_size': 310,
            'images_x_spacing': 2,
            'images_y_spacing': 1,
            'gallery_print_spacing': '9,17,17,17,17',
            'users_print_name_xcoord': 18,
            'page_spacing': 23,
        },
        'misc': {
            'print_info': 'on'
        },
        'experimental': {
            'image_mode_previews': 'off',
            'use_ueberzug': 'off',
            'scroll_display': 'on',
            'ueberzug_center_spaces': 20,
        }
    }

    for action in args:
        if action.name == 'set':
            default[action.section][action.setting] = action.new_setting
        elif action.name == 'delete':
            del default[action.section][action.setting]

    config_object = configparser.ConfigParser()
    config_object.read_dict(default)

    config_path = (path / 'test_config.ini')
    config_path.touch()
    with open(config_path, 'w') as c:
        config_object.write(c)

    return Config(config_path)


