import os
import sys
from pathlib import Path
import configparser

import pytest

# Lmao python
sys.path.append('../koneko/koneko')
sys.path.append('testing')

from koneko import utils

@pytest.fixture
def turn_off_print(monkeypatch):
    monkeypatch.setattr("builtins.print", lambda *a, **k: "")

def test_verify_full_download():
    assert utils.verify_full_download("testing/files/008_77803142_p0.png") == True
    assert utils.verify_full_download("testing/files/not_an_image.txt") == False
    # The above code will remove the file
    os.system("touch testing/files/not_an_image.txt")

@pytest.mark.icat
def test_begin_prompt(monkeypatch):
    # Send a "1" as input
    monkeypatch.setattr("builtins.input", lambda x: "1")
    returned = utils.begin_prompt()
    assert returned == "1"
    os.system("kitty +kitten icat --clear")

def test_artist_user_id_prompt(monkeypatch):
    monkeypatch.setattr("builtins.input",
                        lambda x: "https://www.pixiv.net/en/users/2232374")
    myinput = utils.artist_user_id_prompt()
    assert myinput == "https://www.pixiv.net/en/users/2232374"

@pytest.mark.icat
def test_show_man_loop(monkeypatch, turn_off_print):
    monkeypatch.setattr("builtins.input", lambda x: "")
    monkeypatch.setattr("os.system", lambda x: "")
    utils.show_man_loop()
    os.system("kitty +kitten icat --clear")

def test_clear_cache_loop(monkeypatch, turn_off_print):
    monkeypatch.setattr("shutil.rmtree", lambda x: "")
    monkeypatch.setattr("builtins.input", lambda x: "y")
    monkeypatch.setattr("os.system", lambda x: "")
    utils.clear_cache_loop()
    monkeypatch.setattr("builtins.input", lambda x: "n")
    utils.clear_cache_loop()

@pytest.mark.icat
def test_info_screen_loop(monkeypatch):
    monkeypatch.setattr("builtins.input", lambda x: "")
    monkeypatch.setattr("os.system", lambda x: "")
    utils.info_screen_loop()
    os.system("kitty +kitten icat --clear")

def test_check_noprint(monkeypatch):
    # Apparently monkeypatching the lscat function needs to start with
    # the name of the current module...
    monkeypatch.setattr("koneko.utils.get_settings", lambda x, y: "on")
    assert utils.check_noprint() == True
    monkeypatch.setattr("koneko.utils.get_settings", lambda x, y: "off")
    assert utils.check_noprint() == False
    monkeypatch.setattr("koneko.utils.get_settings", lambda x, y: "false")
    assert utils.check_noprint() == False

def test_noprint(capsys):
    utils.noprint(print, "hello")
    captured = capsys.readouterr()
    assert captured.out == ""

def test_get_settings(monkeypatch):
    # Redivert the config path
    monkeypatch.setattr('koneko.utils.Path.expanduser',
                        lambda x: Path('example_config.ini'))

    assert utils.get_settings('Credentials', 'username') == 'koneko'
    assert utils.get_settings('Credentials', 'password') == '1234'
    assert utils.get_settings('Credentials', 'ID') == '1234'
    assert utils.get_settings('misc', 'experimental') == 'off'
    assert utils.get_settings('misc', 'noprint') == 'off'

def test_config(monkeypatch):
    # If config exists
    example_path = Path('example_config.ini')
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

    creds, your_id = utils.config()
    assert your_id == 'myid'
    assert type(creds) is configparser.SectionProxy

    assert utils.get_settings('Credentials', 'username') == 'myusername'
    assert utils.get_settings('Credentials', 'password') == 'mypassword'

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
