import pytest
from koneko import ui

def test_open_image(monkeypatch):
    monkeypatch.setattr('koneko.ui.os.system', lambda x: True)
    ui.open_in_browser(78823485)
