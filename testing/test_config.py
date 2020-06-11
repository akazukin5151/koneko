import os
import configparser
from pathlib import Path

from returns.result import Success

from koneko import config

import pytest

@pytest.fixture
def use_test_cfg(monkeypatch):
    monkeypatch.setattr('koneko.config.Path.expanduser',
                        lambda x: Path('testing/test_config.ini'))

def test_check_print_info(monkeypatch, use_test_cfg):
    # print_info is on in example config
    assert config.check_print_info() == True

    cfg = configparser.ConfigParser()
    cfg.read('testing/test_config.ini')

    for setting in ('1', 'yes', 'true', 'on'):
        cfg.set('misc', 'print_info', setting)
        with open('testing/test_config.ini', 'w') as f:
            cfg.write(f)
        assert config.check_print_info() == True

    for setting in ('off', 'no', 'off'):
        cfg.set('misc', 'print_info', setting)
        with open('testing/test_config.ini', 'w') as f:
            cfg.write(f)
        assert config.check_print_info() == False

    # Invalid boolean should default to True
    cfg.set('misc', 'print_info',  'asdf')
    with open('testing/test_config.ini', 'w') as f:
        cfg.write(f)
    assert config.check_print_info() == True

    # Restore default value
    cfg.set('misc', 'print_info',  'on')
    with open('testing/test_config.ini', 'w') as f:
        cfg.write(f)

def test_get_settings(monkeypatch, use_test_cfg):
    assert config.get_settings('Credentials', 'username') == Success('koneko')
    assert config.get_settings('Credentials', 'password') == Success('mypassword')
    assert config.get_settings('Credentials', 'ID') == Success('1234')
    assert config.get_settings('experimental', 'image_mode_previews') == Success('off')
    assert config.get_settings('misc', 'print_info') == Success('on')

    # If config doesn't exist
    test_cfg_path = Path('testing/files/test_config.ini')
    if test_cfg_path.exists():
        os.system(f'rm {test_cfg_path}')

    monkeypatch.setattr('koneko.config.Path.expanduser', lambda x: test_cfg_path)

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
