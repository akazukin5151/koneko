import pytest
from koneko import ui

class FakeData:
    def image_id(self):
        return 1

    def url(self):
        return 'fake'

data = FakeData

def test_open_link_coords(monkeypatch):
    monkeypatch.setattr('koneko.ui.os.system', lambda x: True)
    ui.open_link_coords(data, 1, 2)

def test_download_image_coords(monkeypatch):
    monkeypatch.setattr('koneko.api.myapi.protected_download', lambda x: True)
    monkeypatch.setattr('koneko.utils.verify_full_download', lambda x: True)
    monkeypatch.setattr('koneko.download.download_core', lambda *a, **k: True)
    ui.download_image_coords(data, 1, 2)
    # Try if not verified first, then true
    responses = iter([False, True])
    monkeypatch.setattr('koneko.utils.verify_full_download', lambda x: next(responses))
    ui.download_image_coords(data, 1, 2)

def test_previous_page(monkeypatch):
    monkeypatch.setattr('koneko.lscat.show_instant', lambda *a: True)
    monkeypatch.setattr('koneko.pure.print_multiple_imgs', lambda x: True)
    data.current_page_num = 2
    data.current_illusts = None
    ui.previous_page(data)

def test_previous_page_users(monkeypatch):
    monkeypatch.setattr('koneko.lscat.show_instant', lambda *a: True)
    monkeypatch.setattr('koneko.utils.dir_not_empty', lambda *a: True)
    data.page_num = 2
    data.offset = 60
    ui.previous_page_users(data)
    assert data.offset == 30

    data.offset = 60
    monkeypatch.setattr('koneko.utils.dir_not_empty', lambda *a: False)
    assert not ui.previous_page_users(data)
    assert data.offset == 60
