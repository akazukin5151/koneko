import os
import configparser
from unittest.mock import Mock, call

import pytest
from returns.result import Success

from koneko import config
from conftest import setup_test_config, Processer, CustomExit, raises_customexit


defaults = (
    ('lscat', 'page_spacing', 23),
    ('lscat', 'thumbnail_size', 310),
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
    ('lscat', 'thumbnail_size'),
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



def test_users_page_spacing_default(tmp_path):
    testconfig = setup_test_config(tmp_path, config.Config)
    assert testconfig.users_page_spacing() == 20


# Need to set multiple configs
def test_dimension_default(tmp_path):
    testconfig = setup_test_config(tmp_path, config.Config)
    assert testconfig.dimension(config.Dimension.x, (1, 1)) == (18, 2)
    assert testconfig.dimension(config.Dimension.y, (1, 1)) == (8, 1)

dimensions = (
    ('width', 'x', (18, 2)),
    ('height', 'y', (8, 1))
)

@pytest.mark.parametrize('side, dimension, _', dimensions)
def test_set_dimension(tmp_path, side, dimension, _):
    testconfig = setup_test_config(
        tmp_path, config.Config,
        Processer.set('lscat', f'image_{side}', 15),
        Processer.set('lscat', f'images_{dimension}_spacing', 3)
    )
    assert testconfig.dimension(eval(f'config.Dimension.{dimension}'), (1, 1)) == (15, 3)


@pytest.mark.parametrize('action', (Processer.set, Processer.delete))
@pytest.mark.parametrize('side, dimension, fallback', dimensions)
def test_empty_or_invalid_setting(tmp_path, action, side, dimension, fallback):
    testconfig = setup_test_config(
        tmp_path, config.Config,
        action('lscat', f'image_{side}', 'not an int'),
        action('lscat', f'images_{dimension}_spacing', 'not an int')
    )
    assert testconfig.dimension(eval(f'config.Dimension.{dimension}'), fallback) == fallback
    assert testconfig.dimension(eval(f'config.Dimension.{dimension}'), (1, 1)) == (1, 1)



def test_gen_users_settings_default(tmp_path):
    testconfig = setup_test_config(tmp_path, config.Config)
    assert testconfig.gen_users_settings() == (18, 2)


def test_set_gen_users_settings(tmp_path):
    testconfig = setup_test_config(
        tmp_path, config.Config,
        Processer.set('lscat', 'users_print_name_xcoord', 10),
        Processer.set('lscat', 'images_x_spacing', 3)
    )
    assert testconfig.gen_users_settings() == (10, 3)


# Test either one setting missing and both setting missing
settings = (
    'users_print_name_xcoord', 'images_x_spacing',
    ('users_print_name_xcoord', 'images_x_spacing')
)

@pytest.mark.parametrize('action', (Processer.set, Processer.delete))
@pytest.mark.parametrize('setting', settings)
def test_empty_or_invalid_gen_users_settings(tmp_path, action, setting):
    # 'not an int' will be ignored for delete
    if type(setting) == str:
        actions = (action('lscat', setting, 'not an int'),)
    else:
        actions = (
            action('lscat', setting[0], 'not an int'),
            action('lscat', setting[1], 'not an int')
        )

    testconfig = setup_test_config(tmp_path, config.Config, *actions)
    assert testconfig.gen_users_settings() == (18, 2)


# Need to set list of values
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

    creds = config.begin_config()
    assert creds['refresh_token'] == 'token'


def test_first_start(monkeypatch, capsys):
    mock_os = Mock()
    monkeypatch.setattr('koneko.config.os.system', mock_os)
    mock_login = Mock()
    monkeypatch.setattr('koneko.config.login_then_save_verifier', mock_login)
    monkeypatch.setattr('koneko.config.sys.exit', raises_customexit)

    with pytest.raises(CustomExit):
        config.first_start()

    assert mock_os.call_args_list == [
        call('cp ~/.local/share/koneko/pixiv-url.desktop ~/.local/share/applications'),
        call('xdg-mime default pixiv-url.desktop x-scheme-handler/pixiv'),
        call('update-desktop-database ~/.local/share/applications')
    ]
    assert mock_login.called

    captured = capsys.readouterr()
    assert captured.out == 'Please log to pixiv in your browser then run koneko again\n'
