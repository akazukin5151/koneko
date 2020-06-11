import os
import sys
import configparser
from pathlib import Path
from unittest.mock import Mock

import pytest
from returns.maybe import Maybe, Some, Nothing
from returns.result import Result, Success, Failure

from koneko import utils

# Lmao python
sys.path.append('testing')


def test_find_number_map(monkeypatch):
    monkeypatch.setattr('koneko.lscat.ncols_config', lambda: 5)
    assert ([utils.find_number_map(x, y)
             for y in range(1,7)
             for x in range(1,6)] == list(range(30)))
    assert not utils.find_number_map(0, 100)

    monkeypatch.setattr('koneko.lscat.ncols_config', lambda: 6)
    assert [utils.find_number_map(x, y)
            for y in range(1,7)
            for x in range(1,7)][:30] == list(range(30))

def test_cd():
    current_dir = os.getcwd()
    with utils.cd(current_dir):
        testdir = os.getcwd()

    assert testdir == os.getcwd()
    assert os.getcwd() == current_dir

@pytest.fixture
def use_example_cfg(monkeypatch):
    monkeypatch.setattr('koneko.utils.Path.expanduser',
                        lambda x: Path('testing/test_config.ini'))

def test_verify_full_download():
    assert utils.verify_full_download("testing/files/008_77803142_p0.png") == True
    assert utils.verify_full_download("testing/files/not_an_image.txt") == False
    # The above code will remove the file
    os.system("touch testing/files/not_an_image.txt")

def test_check_print_cols(monkeypatch, use_example_cfg):
    # print_cols is off in example config
    assert utils.check_print_cols() == Success(False)

    cfg = configparser.ConfigParser()
    cfg.read('testing/test_config.ini')

    for setting in ('1', 'yes', 'true', 'on'):
        cfg.set('misc', 'print_cols', setting)
        with open('testing/test_config.ini', 'w') as f:
            cfg.write(f)
        assert utils.check_print_cols() == Success(True)

    for setting in ('off', 'no', 'off'):
        cfg.set('misc', 'print_cols', setting)
        with open('testing/test_config.ini', 'w') as f:
            cfg.write(f)
        assert utils.check_print_cols() == Success(False)

    cfg.set('misc', 'print_cols',  'asdf')
    with open('testing/test_config.ini', 'w') as f:
        cfg.write(f)
    assert isinstance(utils.check_print_cols().failure(), ValueError)

    cfg.set('misc', 'print_cols',  'off')
    with open('testing/test_config.ini', 'w') as f:
        cfg.write(f)

def test_get_settings(monkeypatch, use_example_cfg):
    assert utils.get_settings('Credentials', 'username') == Success('koneko')
    assert utils.get_settings('Credentials', 'password') == Success('mypassword')
    assert utils.get_settings('Credentials', 'ID') == Success('1234')
    assert utils.get_settings('experimental', 'image_mode_previews') == Success('off')
    assert utils.get_settings('misc', 'print_cols') == Success('on')

    # If config doesn't exist
    test_cfg_path = Path('testing/files/test_config.ini')
    if test_cfg_path.exists():
        os.system(f'rm {test_cfg_path}')

    monkeypatch.setattr('koneko.utils.Path.expanduser', lambda x: test_cfg_path)

    assert isinstance(utils.get_settings('wewr', 'asda').failure(), KeyError)

def test_config(monkeypatch):
    # If config exists
    example_path = Path('testing/test_config.ini')
    monkeypatch.setattr('koneko.utils.Path.expanduser', lambda x: example_path)

    creds, your_id = utils.config()
    assert your_id == '1234'
    assert type(creds) is configparser.SectionProxy


    # If config doesn't exist
    test_cfg_path = Path('testing/files/test_config.ini')
    if test_cfg_path.exists():
        os.system(f'rm {test_cfg_path}')

    monkeypatch.setattr('koneko.utils.Path.expanduser', lambda x: test_cfg_path)

    # It asks for multiple inputs: username, whether to save user id, user id
    responses = iter(['myusername', 'y', 'myid'])
    monkeypatch.setattr('builtins.input', lambda x='': next(responses))
    monkeypatch.setattr('koneko.utils.getpass', lambda: 'mypassword')
    # fix for macOS
    monkeypatch.setattr('koneko.utils.os.system',
                        lambda x: f'tail example_config.ini -n +9 >> {test_cfg_path}')

    creds, your_id = utils.config()
    assert your_id == 'myid'
    assert type(creds) is configparser.SectionProxy

    assert utils.get_settings('Credentials', 'username') == Success('myusername')
    assert utils.get_settings('Credentials', 'password') == Success('mypassword')

def test_config2(monkeypatch):
    """Config path does not exist, user does not save their ID"""
    test_cfg_path = Path('testing/files/test_config.ini')
    os.system(f'rm {test_cfg_path}')

    monkeypatch.setattr('koneko.utils.Path.expanduser', lambda x: test_cfg_path)

    # It asks for multiple inputs: username, whether to save user id, user id
    responses = iter(['myusername', 'n'])
    monkeypatch.setattr('builtins.input', lambda x='': next(responses))
    monkeypatch.setattr('koneko.utils.getpass', lambda: 'mypassword')
    # fix for macOS
    monkeypatch.setattr('koneko.utils.os.system',
                        lambda x: f'tail example_config.ini -n +9 >> {test_cfg_path}')

    creds, your_id = utils.config()
    assert your_id == ''
    assert type(creds) is configparser.SectionProxy

    assert utils.get_settings('Credentials', 'username') == Success('myusername')
    assert utils.get_settings('Credentials', 'password') == Success('mypassword')


def test_dir_not_empty():
    class FakeData:
        def __init__(self):
            self.download_path = Path('testing/files/')
            self.first_img = "004_祝！！！.jpg"

    # Assert current dir is not empty
    data = FakeData()
    assert utils.dir_not_empty(data)

    # Dir exists but is empty
    Path('testing/files/empty_dir').mkdir()
    data.download_path = Path('testing/files/empty_dir')
    assert utils.dir_not_empty(data) is False

    # .koneko in dir and first image in dir
    os.system('touch testing/files/empty_dir/.koneko')
    os.system('cp testing/files/004_祝！！！.jpg testing/files/empty_dir/')

    assert utils.dir_not_empty(data)

    os.system('rm -r testing/files/empty_dir')

    # Throw some errors
    class FakeData:
        def __init__(self):
            self.download_path = Path('testing/files/')

        @property
        def first_img(self):
            raise KeyError

    data = FakeData()
    assert utils.dir_not_empty(data)

    class FakeData:
        def __init__(self):
            self.download_path = Path('testing/files/')

        @property
        def first_img(self):
            raise AttributeError

    data = FakeData()
    assert utils.dir_not_empty(data)
