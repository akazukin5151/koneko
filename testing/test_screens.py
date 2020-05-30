import sys
from unittest.mock import Mock

import pytest

# Lmao python
sys.path.append('testing')

from koneko import screens

def test_begin_prompt(monkeypatch):
    # pixcat.Image now won't bother us with AttributeErrors and do nothing
    monkeypatch.setattr("koneko.screens.pixcat.Image", lambda *a, **k: Mock())
    # Send a "1" as input
    monkeypatch.setattr("builtins.input", lambda x: "1")
    returned = screens.begin_prompt()
    assert returned == "1"

def test_show_man_loop(monkeypatch, turn_off_print):
    monkeypatch.setattr("koneko.screens.pixcat.Image", lambda *a, **k: Mock())
    monkeypatch.setattr("builtins.input", lambda x: "")
    screens.show_man_loop()

def test_clear_cache_loop(monkeypatch, turn_off_print):
    monkeypatch.setattr("shutil.rmtree", lambda x: "")
    monkeypatch.setattr("builtins.input", lambda x: "y")
    screens.clear_cache_loop()
    monkeypatch.setattr("builtins.input", lambda x: "n")
    screens.clear_cache_loop()

def test_info_screen_loop(monkeypatch):
    monkeypatch.setattr("koneko.screens.pixcat.Image", lambda *a, **k: Mock())
    monkeypatch.setattr("builtins.input", lambda x: "")
    screens.info_screen_loop()
