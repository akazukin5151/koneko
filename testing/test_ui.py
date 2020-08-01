"""These are also essentially integration tests to test the code properly organises work
Code that does work has already been tested
"""

from pathlib import Path
from unittest.mock import Mock

import pytest

from koneko import ui, utils, download


class FakeData:
    def image_id(self):
        return 1

    def url(self):
        return 'fake'

data = FakeData

def test_open_link_coords(monkeypatch, capsys):
    monkeypatch.setattr('koneko.ui.os.system', lambda x: True)
    utils.open_link_coords(data, 1, 2)

    captured = capsys.readouterr()
    assert captured.out == 'Opened https://www.pixiv.net/artworks/1 in browser!\n'

def test_download_image_coords(monkeypatch, capsys):
    # Should belong to test_download.py now
    monkeypatch.setattr('koneko.api.myapi.protected_download', lambda x: True)
    monkeypatch.setattr('koneko.files.verify_full_download', lambda x: True)
    monkeypatch.setattr('koneko.download.download_url', lambda *a, **k: True)
    download.download_image_coords(data, 1, 2)
    # Try if not verified first, then true
    responses = iter([False, True])
    monkeypatch.setattr('koneko.files.verify_full_download', lambda x: next(responses))
    download.download_image_coords(data, 1, 2)

    captured = capsys.readouterr()
    assert captured.out == f'Image downloaded at {Path("~/Downloads/fake").expanduser()}\n' * 2


def test_previous_page_gallery(monkeypatch):
    monkeypatch.setattr('koneko.files.dir_not_empty', lambda x: True)
    monkeypatch.setattr('koneko.lscat.show_instant', lambda *a: True)

    class FakeGallery:
        def __init__(self):
            self._data = data
            self._tracker_class = Mock()
            self.canvas = None
            self.scrollable = False
        def _show_page(self):
            ui.AbstractGallery._show_page(self)
        def _print_page_info(self, *a):
            return True
        def scroll_or_show(self):
            pass

    gallery = FakeGallery()
    gallery._data.page_num = 2
    gallery._data.current_illusts = None
    gallery._data.offset = 60
    gallery._data.download_path = 'fake'
    ui.AbstractGallery.previous_page(gallery)


def test_previous_page_users(monkeypatch, capsys):
    monkeypatch.setattr('koneko.files.dir_not_empty', lambda *a: True)
    monkeypatch.setattr('koneko.lscat.show_instant', lambda *a: True)

    class FakeGallery:
        def __init__(self):
            self._data = data
            self._tracker_class = Mock()
            self.canvas = None
            self.scrollable = False
        def _show_page(self):
            ui.AbstractGallery._show_page(self)
        def _print_page_info(self, *a):
            return True
        def scroll_or_show(self):
            pass


    gallery = FakeGallery()
    gallery._data.page_num = 2
    gallery._data.offset = 60
    ui.AbstractGallery.previous_page(gallery)
    assert gallery._data.offset == 30

    gallery._data.offset = 60
    monkeypatch.setattr('koneko.files.dir_not_empty', lambda *a: False)
    assert not ui.AbstractUsers.previous_page(gallery)
    assert gallery._data.offset == 60

    captured = capsys.readouterr()
    assert captured.out == 'This is the first page!\n'


def test_show_full_res(monkeypatch):
    monkeypatch.setattr('koneko.download.download_url', lambda *a: True)
    monkeypatch.setattr('koneko.lscat.icat', lambda *a: True)
    data.current_url = 'fake'
    data.download_path = Path('fake')
    ui.Image.show_full_res(data)

def test_next_image(monkeypatch, capsys):
    monkeypatch.setattr('koneko.download.async_download_spinner', lambda *a: True)
    monkeypatch.setattr('koneko.lscat.icat', lambda *a: True)
    monkeypatch.setattr('koneko.ui.Image.start_preview', lambda *a: True)
    data.page_urls = False
    data.event = Mock()
    data.jump_to_image = Mock()

    # Only image
    data.number_of_pages = 1
    assert not ui.Image.next_image(data)

    captured = capsys.readouterr()
    assert captured.out == 'This is the only image in the post!\n'

    # Last image
    data.page_urls = 'fake'
    data.number_of_pages = 2
    assert not ui.Image.next_image(data)

    data.image_filename = 'fake'
    data.filepath = 'fake'
    data.number_of_pages = 10
    ui.Image.next_image(data)

    captured = capsys.readouterr()
    assert captured.out == 'This is the last image in the post!\n'


def test_previous_image(monkeypatch, capsys):
    monkeypatch.setattr('koneko.download.async_download_spinner', lambda *a: True)
    monkeypatch.setattr('koneko.lscat.icat', lambda *a: True)
    monkeypatch.setattr('koneko.ui.Image.start_preview', lambda *a: True)
    data.page_urls = False
    data.event = Mock()
    data.jump_to_image = Mock()

    # Only image
    data.number_of_pages = 1
    assert not ui.Image.previous_image(data)

    captured = capsys.readouterr()
    assert captured.out == 'This is the only image in the post!\n'

    # First image
    data.page_urls = 'fake'
    data.page_num = 0
    assert not ui.Image.previous_image(data)

    data.image_filename = 'fake'
    data.filepath = 'fake'
    data.number_of_pages = 10
    data.page_num = 2
    ui.Image.previous_image(data)

    captured = capsys.readouterr()
    assert captured.out == 'This is the first image in the post!\n'


def test_prefetch_next_image(monkeypatch):
    monkeypatch.setattr('koneko.download.async_download_spinner', lambda *a: True)
    data.next_img_url = 'fake'
    ui.Image._prefetch_next_image(data)
