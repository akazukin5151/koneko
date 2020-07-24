import sys
from unittest.mock import Mock

import pytest

from koneko import screens

# Lmao python
sys.path.append('testing')


@pytest.fixture()
def turn_off_print(monkeypatch):
    monkeypatch.setattr('builtins.print', lambda *a, **k: '')

@pytest.fixture
def disable_pixcat(monkeypatch):
    # pixcat.Image now won't bother us with AttributeErrors and do nothing
    monkeypatch.setattr('koneko.screens.pixcat.Image', lambda *a, **k: Mock())

def test_begin_prompt(monkeypatch, disable_pixcat, capsys):
    # Send a "1" as input
    monkeypatch.setattr('builtins.input', lambda x: '1')
    returned = screens.begin_prompt()
    assert returned == '1'

    captured = capsys.readouterr()
    assert captured.out[-100:] == 'ual\n                               b. Browse cache (offline)\n                               q. Quit\n'


def test_show_man_loop(monkeypatch, turn_off_print, disable_pixcat, send_enter):
    screens.show_man_loop()

def test_clear_cache_loop(monkeypatch, turn_off_print):
    monkeypatch.setattr('shutil.rmtree', lambda x: '')
    monkeypatch.setattr('builtins.input', lambda x: 'y')
    screens.clear_cache_loop()
    monkeypatch.setattr('builtins.input', lambda x: 'n')
    screens.clear_cache_loop()

def test_info_screen_loop(monkeypatch, disable_pixcat, send_enter, capsys):
    screens.info_screen_loop()

    captured = capsys.readouterr()
    assert captured.out[-100:] == 'works/71471144\n                           Current image: https://www.pixiv.net/en/artworks/79494300\n'
