import os
import configparser

import pytest
from returns.result import Success

from koneko import config
from conftest import setup_test_config


def write_print_setting(cfg, setting, tmp_path):
    cfg.set('misc', 'print_info', setting)
    with open(tmp_path / 'test_config.ini', 'w') as f:
        cfg.write(f)


#def test_check_print_info_default(tmp_path):
#    setup_test_config(tmp_path)
#    assert config.check_print_info() is True
#
#
#@pytest.mark.parametrize('setting', ('1', 'yes', 'true', 'on'))
#def test_check_print_info_true(tmp_path, setting, use_test_cfg_path):
#    cfg = setup_test_config(tmp_path)
#    write_print_setting(cfg, setting, tmp_path)
#    assert config.check_print_info() is True
#
#
#@pytest.mark.parametrize('setting', ('off', 'no', 'off'))
#def test_check_print_info_false(tmp_path, setting, use_test_cfg_path):
#    cfg = setup_test_config(tmp_path)
#    write_print_setting(cfg, setting, tmp_path)
#    assert config.check_print_info() is False
#
#
#def test_check_print_info_invalid_true(tmp_path, use_test_cfg_path):
#    cfg = setup_test_config(tmp_path)
#    write_print_setting(cfg, 'not_a_boolean', tmp_path)
#    assert config.check_print_info() is True


# Defaults
def test_scroll_display_new(tmp_path):
    testconfig = setup_test_config(tmp_path, config.Config)
    assert testconfig.scroll_display() is True

def test_use_ueberzug(tmp_path):
    testconfig = setup_test_config(tmp_path, config.Config)
    assert testconfig.use_ueberzug() is False

def test_check_image_preview_new(tmp_path):
    testconfig = setup_test_config(tmp_path, config.Config)
    assert testconfig.check_image_preview() is False

def test_check_print_info_default_new(tmp_path):
    testconfig = setup_test_config(tmp_path, config.Config)
    assert testconfig.check_print_info() is True

def test_gallery_page_spacing(tmp_path):
    testconfig = setup_test_config(tmp_path, config.Config)
    assert testconfig.gallery_page_spacing_config() == 23

def test_users_page_spacing(tmp_path):
    testconfig = setup_test_config(tmp_path, config.Config)
    assert testconfig.users_page_spacing_config() == 20

def test_thumbnail_size_config(tmp_path):
    testconfig = setup_test_config(tmp_path, config.Config)
    assert testconfig.thumbnail_size_config() == 310

def test_get_gen_users_settings(tmp_path):
    testconfig = setup_test_config(tmp_path, config.Config)
    assert testconfig.get_gen_users_settings() == (18, 2)

def test_gallery_print_spacing_config_default_new(tmp_path):
    testconfig = setup_test_config(tmp_path, config.Config)
    assert testconfig.gallery_print_spacing_config() == ['9', '17', '17', '17', '17']

def test_ueberzug_center_spaces_new(tmp_path):
    testconfig = setup_test_config(tmp_path, config.Config)
    assert testconfig.ueberzug_center_spaces() == 20

def test_dimension(tmp_path):
    testconfig = setup_test_config(tmp_path, config.Config)
    assert testconfig.dimension(config.Dimension.x, (1, 1)) == (18, 2)
    assert testconfig.dimension(config.Dimension.y, (1, 1)) == (8, 1)


def test_begin_config_exists(monkeypatch, tmp_path, use_test_cfg_path):
    testconfig = setup_test_config(tmp_path, config.Config)
    monkeypatch.setattr('koneko.config.api', testconfig)

    creds, your_id = config.begin_config()
    assert your_id == '1234'
    assert creds['username'] == 'koneko'
    assert creds['password'] == 'mypassword'


@pytest.mark.parametrize('testid, responses', (('myid', ['y', 'myid']), ('', ['n'])))
def test_begin_config_nonexistant_id(monkeypatch, tmp_path, use_test_cfg_path, capsys, testid, responses):
    """Config path does not exist"""
    responses = iter(['myusername'] + responses)
    monkeypatch.setattr('builtins.input', lambda *a: next(responses))
    monkeypatch.setattr('koneko.config.getpass', lambda *a: 'mypassword')
    # fix for macOS
    monkeypatch.setattr(
        'koneko.config.os.system',
        lambda x: f'tail example_config.ini -n +9 >> {tmp_path / "test_config.ini"}'
    )

    creds, your_id = config.begin_config()
    assert your_id == testid
    assert creds['username'] == 'myusername'
    assert creds['password'] == 'mypassword'

    testconfig = config.Config(tmp_path / 'test_config.ini')
    assert testconfig.get_setting('Credentials', 'username') == Success('myusername')
    assert testconfig.get_setting('Credentials', 'password') == Success('mypassword')

    captured = capsys.readouterr()
    assert captured.out == '\nDo you want to save your pixiv ID? It will be more convenient\nto view artists you are following\n'
