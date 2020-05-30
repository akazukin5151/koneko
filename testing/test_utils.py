import os
import sys
from pathlib import Path
import configparser
from unittest.mock import Mock

import pytest

# Lmao python
sys.path.append('testing')

from koneko import utils

@pytest.fixture(scope='module')
def turn_off_print(monkeypatch):
    monkeypatch.setattr("builtins.print", lambda *a, **k: "")

@pytest.fixture
def use_example_cfg(monkeypatch):
    monkeypatch.setattr('koneko.utils.Path.expanduser',
                        lambda x: Path('testing/test_config.ini'))

def test_verify_full_download():
    assert utils.verify_full_download("testing/files/008_77803142_p0.png") == True
    assert utils.verify_full_download("testing/files/not_an_image.txt") == False
    # The above code will remove the file
    os.system("touch testing/files/not_an_image.txt")

def test_check_noprint(monkeypatch, use_example_cfg):
    # noprint is off in example config
    assert utils.check_noprint() == False

    cfg = configparser.ConfigParser()
    cfg.read('testing/test_config.ini')

    for setting in ('1', 'yes', 'true', 'on'):
        cfg.set('misc', 'noprint', setting)
        with open('testing/test_config.ini', 'w') as f:
            cfg.write(f)
        assert utils.check_noprint() == True

    for setting in ('off', 'no', 'asdf'):
        cfg.set('misc', 'noprint', setting)
        with open('testing/test_config.ini', 'w') as f:
            cfg.write(f)
        assert utils.check_noprint() == False

    # Restore default
    cfg.set('misc', 'noprint', 'off')
    with open('testing/test_config.ini', 'w') as f:
        cfg.write(f)

def test_noprint(capsys):
    utils.noprint(print, "hello")
    captured = capsys.readouterr()
    assert captured.out == ""

def test_get_settings(monkeypatch, use_example_cfg):
    assert utils.get_settings('Credentials', 'username') == 'koneko'
    assert utils.get_settings('Credentials', 'password') == 'mypassword'
    assert utils.get_settings('Credentials', 'ID') == '1234'
    assert utils.get_settings('experimental', 'image_mode_previews') == 'off'
    assert utils.get_settings('misc', 'noprint') == 'off'

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
    monkeypatch.setattr('builtins.input', lambda x=None: next(responses))
    monkeypatch.setattr('koneko.utils.getpass', lambda: 'mypassword')
    # fix for macOS
    monkeypatch.setattr('koneko.utils.os.system',
                        lambda x: f'tail example_config.ini -n +9 >> {test_cfg_path}')

    creds, your_id = utils.config()
    assert your_id == 'myid'
    assert type(creds) is configparser.SectionProxy

    try:
        assert utils.get_settings('Credentials', 'username') == 'myusername'
        assert utils.get_settings('Credentials', 'password') == 'mypassword'
    except configparser.DuplicateSectionError:
        os.system(f'cat {test_cfg_path}')


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
