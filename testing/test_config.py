import os
import configparser
from pathlib import Path

import pytest
from returns.result import Success

from koneko import config


@pytest.fixture
def use_test_cfg_path(monkeypatch, tmp_path):
    monkeypatch.setattr('koneko.config.Path.expanduser',
                        lambda x: Path(tmp_path / 'test_config.ini'))


def setup_test_config(path):
    default = """[Credentials]
    username = koneko
    password = mypassword
    id = 1234

    [lscat]
    image_width = 18
    image_height = 8
    image_thumbnail_size = 310
    images_x_spacing = 2
    images_y_spacing = 1
    gallery_print_spacing = 9,17,17,17,17
    users_print_name_xcoord = 18
    gallery_page_spacing = 23
    users_page_spacing = 20

    [misc]
    print_info = on

    [experimental]
    image_mode_previews = off
    """
    config_object = configparser.ConfigParser()
    config_object.read_string(default)

    config_path = (path / 'test_config.ini')
    config_path.touch()
    with open(config_path, 'w') as c:
        config_object.write(c)
    return config_object


def write_print_setting(cfg, setting, tmp_path):
    cfg.set('misc', 'print_info', setting)
    with open(tmp_path / 'test_config.ini', 'w') as f:
        cfg.write(f)


def test_check_print_info_default(tmp_path):
    setup_test_config(tmp_path)
    assert config.check_print_info() is True


@pytest.mark.parametrize('setting', ('1', 'yes', 'true', 'on'))
def test_check_print_info_true(tmp_path, setting, use_test_cfg_path):
    cfg = setup_test_config(tmp_path)
    write_print_setting(cfg, setting, tmp_path)
    assert config.check_print_info() is True


@pytest.mark.parametrize('setting', ('off', 'no', 'off'))
def test_check_print_info_false(tmp_path, setting, use_test_cfg_path):
    cfg = setup_test_config(tmp_path)
    write_print_setting(cfg, setting, tmp_path)
    assert config.check_print_info() is False


def test_check_print_info_invalid_true(tmp_path, use_test_cfg_path):
    cfg = setup_test_config(tmp_path)
    write_print_setting(cfg, 'not_a_boolean', tmp_path)
    assert config.check_print_info() is True


def test_get_settings_default(tmp_path, use_test_cfg_path):
    setup_test_config(tmp_path)
    assert config.get_settings('Credentials', 'username') == Success('koneko')
    assert config.get_settings('Credentials', 'password') == Success('mypassword')
    assert config.get_settings('Credentials', 'ID') == Success('1234')
    assert config.get_settings('experimental', 'image_mode_previews') == Success('off')
    assert config.get_settings('misc', 'print_info') == Success('on')


def test_get_settings_nonexistent(use_test_cfg_path):
    assert isinstance(config.get_settings('wewr', 'asda').failure(), KeyError)


def test_config(monkeypatch):
    # If config exists
    example_path = Path('testing/test_config.ini')
    monkeypatch.setattr('koneko.config.Path.expanduser', lambda x: example_path)

    creds, your_id = config.begin_config()
    assert your_id == '1234'
    assert type(creds) is configparser.SectionProxy


    # If config doesn't exist
    test_cfg_path = Path('testing/files/test_config.ini')
    if test_cfg_path.exists():
        os.system(f'rm {test_cfg_path}')

    monkeypatch.setattr('koneko.config.Path.expanduser', lambda x: test_cfg_path)

    # It asks for multiple inputs: username, whether to save user id, user id
    responses = iter(['myusername', 'y', 'myid'])
    monkeypatch.setattr('builtins.input', lambda x='': next(responses))
    monkeypatch.setattr('koneko.config.getpass', lambda: 'mypassword')
    # fix for macOS
    monkeypatch.setattr('koneko.config.os.system',
                        lambda x: f'tail example_config.ini -n +9 >> {test_cfg_path}')

    creds, your_id = config.begin_config()
    assert your_id == 'myid'
    assert type(creds) is configparser.SectionProxy

    assert config.get_settings('Credentials', 'username') == Success('myusername')
    assert config.get_settings('Credentials', 'password') == Success('mypassword')

def test_config2(monkeypatch):
    """Config path does not exist, user does not save their ID"""
    test_cfg_path = Path('testing/files/test_config.ini')
    os.system(f'rm {test_cfg_path}')

    monkeypatch.setattr('koneko.config.Path.expanduser', lambda x: test_cfg_path)

    # It asks for multiple inputs: username, whether to save user id, user id
    responses = iter(['myusername', 'n'])
    monkeypatch.setattr('builtins.input', lambda x='': next(responses))
    monkeypatch.setattr('koneko.config.getpass', lambda: 'mypassword')
    # fix for macOS
    monkeypatch.setattr('koneko.config.os.system',
                        lambda x: f'tail example_config.ini -n +9 >> {test_cfg_path}')

    creds, your_id = config.begin_config()
    assert your_id == ''
    assert type(creds) is configparser.SectionProxy

    assert config.get_settings('Credentials', 'username') == Success('myusername')
    assert config.get_settings('Credentials', 'password') == Success('mypassword')
