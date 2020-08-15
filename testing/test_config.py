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
)


@pytest.mark.parametrize('_, method, expected', defaults)
def test_method_defaults(tmp_path, _, method, expected):
    testconfig = setup_test_config(tmp_path, config.Config)
    assert eval(f'testconfig.{method}()') == expected


@pytest.mark.parametrize('action', (Processer.set, Processer.delete))
@pytest.mark.parametrize('section, method, fallback', defaults)
def test_empty_or_invalid_setting(tmp_path, action, section, method, fallback):
    testconfig = setup_test_config(
        tmp_path, config.Config,
        action(section, method, 'not a boolean; not an int or list either')
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
        Processer.set(section, method, setting)
    )
    assert eval(f'testconfig.{method}()') is True


@pytest.mark.parametrize('setting', ('off', 'no', 'false', '0'))
@pytest.mark.parametrize('section, method', boolean_settings)
def test_set_boolean_to_false(tmp_path, setting, section, method):
    testconfig = setup_test_config(
        tmp_path, config.Config,
        Processer.set(section, method, setting)
    )
    assert eval(f'testconfig.{method}()') is False


int_settings = (
    ('lscat', 'page_spacing'),
    ('lscat', 'image_thumbnail_size'),
    ('experimental', 'ueberzug_center_spaces'),
)

@pytest.mark.parametrize('setting', range(10,2))
@pytest.mark.parametrize('section, method', int_settings)
def test_set_ints(tmp_path, setting, section, method):
    testconfig = setup_test_config(
        tmp_path, config.Config,
        Processer.set(section, method, setting)
    )
    assert eval(f'testconfig.{method}()') == setting



# Slightly more complicated methods return multiple configs, or derived from another
# TODO: invalid and empty settings
def test_dimension_default(tmp_path):
    testconfig = setup_test_config(tmp_path, config.Config)
    assert testconfig.dimension(config.Dimension.x, (1, 1)) == (18, 2)
    assert testconfig.dimension(config.Dimension.y, (1, 1)) == (8, 1)

# Set
# Empty
# Invalid

# TODO: Users page spacing
# Set
# Empty
# Invalid

def test_get_gen_users_settings_default(tmp_path):
    testconfig = setup_test_config(tmp_path, config.Config)
    assert testconfig.get_gen_users_settings() == (18, 2)


def test_set_get_gen_users_settings(tmp_path):
    testconfig = setup_test_config(
        tmp_path, config.Config,
        Processer.set('lscat', 'users_print_name_xcoord', 10),
        Processer.set('lscat', 'images_x_spacing', 3)
    )
    assert testconfig.get_gen_users_settings() == (10, 3)


# Test either one setting missing and both setting missing
settings = (
    'users_print_name_xcoord', 'images_x_spacing',
    ('users_print_name_xcoord', 'images_x_spacing')
)

@pytest.mark.parametrize('action', (Processer.set, Processer.delete))
@pytest.mark.parametrize('setting', settings)
def test_empty_or_invalid_get_gen_users_settings(tmp_path, action, setting):
    # 'not an int' will be ignored for delete
    if type(setting) == str:
        actions = (action('lscat', setting, 'not an int'),)
    else:
        actions = (
            action('lscat', setting[0], 'not an int'),
            action('lscat', setting[1], 'not an int')
        )

    testconfig = setup_test_config(tmp_path, config.Config, *actions)
    assert testconfig.get_gen_users_settings() == (18, 2)



def test_gallery_print_spacing_default(tmp_path):
    testconfig = setup_test_config(tmp_path, config.Config)
    assert testconfig.gallery_print_spacing() == [9, 17, 17, 17, 17]


@pytest.mark.parametrize('setting', [range(0,10,2), range(0,20,5)])
def test_set_gallery_print_spacing(tmp_path, setting):
    testconfig = setup_test_config(
        tmp_path, config.Config,
        Processer.set('lscat', 'gallery_print_spacing',
        ','.join([str(x) for x in list(setting)]))
    )
    assert testconfig.gallery_print_spacing() == list(setting)


@pytest.mark.parametrize('action', (Processer.set, Processer.delete))
def test_gallery_print_spacing_empty_or_invalid(tmp_path, action):
    testconfig = setup_test_config(tmp_path, config.Config,
        action('lscat', 'gallery_print_spacing', 'not an int')
    )
    assert testconfig.gallery_print_spacing() == [9, 17, 17, 17, 17]


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
