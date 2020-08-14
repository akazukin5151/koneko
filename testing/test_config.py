import os
import configparser

import pytest
from returns.result import Success

from koneko import config
from conftest import setup_test_config, Processer


defaults = (
    ('lscat', 'page_spacing', 23),
    ('lscat', 'image_thumbnail_size', 310),
    ('lscat', 'gallery_print_spacing', [9, 17, 17, 17, 17]),
    ('misc', 'print_info', True),
    ('experimental', 'use_ueberzug', False),
    ('experimental', 'scroll_display', True),
    ('experimental', 'image_mode_previews', False),
    ('experimental', 'ueberzug_center_spaces', 20),
    #('lscat', 'users_page_spacing', 20),
    #('lscat', 'get_gen_users_settings', (18, 2)),
)


@pytest.mark.parametrize('_, method, expected', defaults)
def test_method_defaults(tmp_path, _, method, expected):
    testconfig = setup_test_config(tmp_path, config.Config)
    assert eval(f'testconfig.{method}()') == expected


def test_dimension_default(tmp_path):
    testconfig = setup_test_config(tmp_path, config.Config)
    assert testconfig.dimension(config.Dimension.x, (1, 1)) == (18, 2)
    assert testconfig.dimension(config.Dimension.y, (1, 1)) == (8, 1)


@pytest.mark.parametrize('section, method, fallback', defaults)
def test_invalid_setting_fallbacks_to_default(tmp_path, section, method, fallback):
    testconfig = setup_test_config(
        tmp_path, config.Config,
        Processer.set, section, method, 'not a boolean; not an int or list either'
    )
    assert eval(f'testconfig.{method}()') == fallback


@pytest.mark.parametrize('section, method, fallback', defaults)
def test_empty_setting_fallbacks_to_default(tmp_path, section, method, fallback):
    testconfig = setup_test_config(
        tmp_path, config.Config,
        Processer.delete, section, method
    )
    assert eval(f'testconfig.{method}()') == fallback


boolean_settings = (
    ('misc', 'print_info'),
    ('experimental', 'use_ueberzug'),
    ('experimental', 'scroll_display'),
    ('experimental', 'image_mode_previews')
)


@pytest.mark.parametrize('setting', ('1', 'yes', 'true', 'on'))
@pytest.mark.parametrize('section, method', boolean_settings)
def test_set_boolean_to_true(tmp_path, setting, section, method):
    testconfig = setup_test_config(
        tmp_path, config.Config,
        Processer.set, section, method, setting
    )
    assert eval(f'testconfig.{method}()') is True


@pytest.mark.parametrize('setting', ('off', 'no', 'false', '0'))
@pytest.mark.parametrize('section, method', boolean_settings)
def test_set_boolean_to_false(tmp_path, setting, section, method):
    testconfig = setup_test_config(
        tmp_path, config.Config,
        Processer.set, section, method, setting
    )
    assert eval(f'testconfig.{method}()') is False



def test_get_gen_users_settings(tmp_path):
    testconfig = setup_test_config(tmp_path, config.Config)
    assert testconfig.get_gen_users_settings() == (18, 2)

def test_gallery_print_spacing_config_default(tmp_path):
    testconfig = setup_test_config(tmp_path, config.Config)
    assert testconfig.gallery_print_spacing_config() == [9, 17, 17, 17, 17]



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
