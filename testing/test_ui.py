from pathlib import Path

import pytest

from koneko import ui, utils, download


class FakeData:
    def image_id(self):
        return 1

    def url(self):
        return 'fake'

data = FakeData

def test_open_link_coords(monkeypatch):
    monkeypatch.setattr('koneko.ui.os.system', lambda x: True)
    utils.open_link_coords(data, 1, 2)

def test_download_image_coords(monkeypatch):
    # Should belong to test_download.py now
    monkeypatch.setattr('koneko.api.myapi.protected_download', lambda x: True)
    monkeypatch.setattr('koneko.utils.verify_full_download', lambda x: True)
    monkeypatch.setattr('koneko.download.download_core', lambda *a, **k: True)
    download.download_image_coords(data, 1, 2)
    # Try if not verified first, then true
    responses = iter([False, True])
    monkeypatch.setattr('koneko.utils.verify_full_download', lambda x: next(responses))
    download.download_image_coords(data, 1, 2)

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

def test_show_full_res(monkeypatch):
    monkeypatch.setattr('koneko.download.download_core', lambda *a: True)
    monkeypatch.setattr('koneko.lscat.icat', lambda *a: True)
    data.current_url = 'fake'
    data.download_path = Path('fake')
    ui.show_full_res(data)

def test_next_image(monkeypatch):
    monkeypatch.setattr('koneko.download.async_download_spinner', lambda *a: True)
    monkeypatch.setattr('koneko.lscat.icat', lambda *a: True)
    data.page_urls = False

    # Only image
    data.number_of_pages = 1
    assert not ui.next_image(data)

    # Last image
    data.page_urls = 'fake'
    data.number_of_pages = 2
    assert not ui.next_image(data)

    data.image_filename = 'fake'
    data.filepath = 'fake'
    data.number_of_pages = 10
    ui.next_image(data)

def test_previous_image(monkeypatch):
    monkeypatch.setattr('koneko.download.async_download_spinner', lambda *a: True)
    monkeypatch.setattr('koneko.lscat.icat', lambda *a: True)
    data.page_urls = False

    # Only image
    data.number_of_pages = 1
    assert not ui.previous_image(data)

    # First image
    data.page_urls = 'fake'
    data.page_num = 0
    assert not ui.previous_image(data)

    data.image_filename = 'fake'
    data.filepath = 'fake'
    data.number_of_pages = 10
    data.page_num = 2
    ui.previous_image(data)

def test_prefetch_next_image(monkeypatch):
    monkeypatch.setattr('koneko.download.async_download_spinner', lambda *a: True)
    data.next_img_url = 'fake'
    ui._prefetch_next_image(data)
