import configparser

import pytest
from returns.result import Success

from koneko import config
from conftest import setup_test_config


def write_print_setting(cfg, setting, tmp_path):
    cfg.set('misc', 'print_info', setting)
    with open(tmp_path / 'test_config.ini', 'w') as f:
        cfg.write(f)


def test_scroll_display(tmp_path, use_test_cfg_path):
    setup_test_config(tmp_path)
    assert config.scroll_display() is True


def test_ueberzug_center_spaces(tmp_path, use_test_cfg_path):
    setup_test_config(tmp_path)
    assert config.ueberzug_center_spaces() == 20


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


def test_begin_config_exists(monkeypatch, tmp_path, use_test_cfg_path):
    setup_test_config(tmp_path)
    creds, your_id = config.begin_config()
    assert your_id == '1234'
    assert type(creds) is configparser.SectionProxy


def test_begin_config_nonexistant_id(monkeypatch, tmp_path, use_test_cfg_path, capsys):
    """Config path does not exist, user saves their ID"""
    # It asks for multiple inputs: username, whether to save user id, user id
    responses = iter(['myusername', 'y', 'myid'])
    monkeypatch.setattr('builtins.input', lambda x='': next(responses))
    monkeypatch.setattr('koneko.config.getpass', lambda: 'mypassword')
    # fix for macOS
    monkeypatch.setattr(
        'koneko.config.os.system',
        lambda x: f'tail example_config.ini -n +9 >> {tmp_path / "test_config.ini"}'
    )

    creds, your_id = config.begin_config()
    assert your_id == 'myid'
    assert type(creds) is configparser.SectionProxy

    assert config.get_settings('Credentials', 'username') == Success('myusername')
    assert config.get_settings('Credentials', 'password') == Success('mypassword')

    captured = capsys.readouterr()
    assert captured.out == '\nPlease enter your password:\n\nDo you want to save your pixiv ID? It will be more convenient\nto view artists you are following\n'


def test_begin_config_nonexistant_no_id(monkeypatch, tmp_path, use_test_cfg_path, capsys):
    """Config path does not exist, user does not save their ID"""
    # It asks for multiple inputs: username, whether to save user id, user id
    responses = iter(['myusername', 'n'])
    monkeypatch.setattr('builtins.input', lambda x='': next(responses))
    monkeypatch.setattr('koneko.config.getpass', lambda: 'mypassword')
    # fix for macOS
    monkeypatch.setattr(
        'koneko.config.os.system',
        lambda x: f'tail example_config.ini -n +9 >> {tmp_path / "test_config.ini"}'
    )

    creds, your_id = config.begin_config()
    assert your_id == ''
    assert type(creds) is configparser.SectionProxy

    assert config.get_settings('Credentials', 'username') == Success('myusername')
    assert config.get_settings('Credentials', 'password') == Success('mypassword')

    captured = capsys.readouterr()
    assert captured.out == '\nPlease enter your password:\n\nDo you want to save your pixiv ID? It will be more convenient\nto view artists you are following\n'


def test_gallery_print_spacing_config(tmp_path, use_test_cfg_path):
    setup_test_config(tmp_path)
    assert config.gallery_print_spacing_config() == ['9', '17', '17', '17', '17']

def test_gallery_print_spacing_config_default(use_test_cfg_path):
    assert config.gallery_print_spacing_config() == ['9', '17', '17', '17', '17']


def test_check_image_preview(tmp_path, use_test_cfg_path):
    setup_test_config(tmp_path)
    assert config.check_image_preview() is False
