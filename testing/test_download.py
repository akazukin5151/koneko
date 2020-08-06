from pathlib import Path
from collections import namedtuple
from unittest.mock import Mock, call

import pytest

from koneko import download


def test_save_number_of_artists(tmp_path):
    FakeData = namedtuple('data', ('download_path', 'splitpoint'))
    data = FakeData(tmp_path, 30)
    download.save_number_of_artists(data)
    with open(tmp_path / '.koneko', 'r') as f:
        assert f.read() == '30'


def test_init_download_dir_has_files(monkeypatch):
    monkeypatch.setattr('koneko.files.dir_not_empty', lambda x: True)
    assert download.init_download(None, None) is True

def test_init_download(monkeypatch):
    mocked_api = Mock()
    mocked_rmtree = Mock()
    mocked_url = Mock()
    monkeypatch.setattr('koneko.api.myapi', mocked_api)
    monkeypatch.setattr('koneko.files.dir_not_empty', lambda x: False)
    monkeypatch.setattr('koneko.download.rmtree', mocked_rmtree)
    monkeypatch.setattr('koneko.download.itertools.filterfalse', lambda *a: [mocked_url]*2)
    monkeypatch.setattr('koneko.download.os.makedirs', lambda *a, **k: True)  # Disable

    mocked_data = Mock()
    mocked_data.all_urls = [Mock()] * 2
    mocked_tracker = Mock()
    download.init_download(mocked_data, mocked_tracker)

    assert mocked_rmtree.call_args_list == [call(mocked_data.download_path)]
    assert mocked_data.method_calls == [call.download_path.is_dir()]
    assert 'page_num' in dir(mocked_data)
    assert 'newnames_with_ext' in dir(mocked_data)

    assert mocked_tracker.method_calls == [call.update(mocked_url)] * 2

    assert mocked_api.method_calls == [
        call.protected_download(mocked_data.all_urls[0], mocked_data.download_path, mocked_url),
        call.protected_download(mocked_data.all_urls[1], mocked_data.download_path, mocked_url)
    ]


def test_async_download_no_rename(monkeypatch, tmp_path):
    mocked_url = Mock()
    mocked_tracker = Mock()
    mocked_api = Mock()
    monkeypatch.setattr('koneko.api.myapi', mocked_api)

    download.async_download_no_rename(tmp_path, [mocked_url] * 2, mocked_tracker)

    assert mocked_tracker.method_calls == [call.update(None)] * 2
    assert mocked_api.method_calls == [call.protected_download(mocked_url, tmp_path, None)] * 2
    assert tmp_path.exists()


def test_download_url(monkeypatch, tmp_path, capsys):
    mocked_api = Mock()
    monkeypatch.setattr('koneko.api.myapi', mocked_api)

    download.download_url(tmp_path, 'url', tmp_path / 'filename')

    assert tmp_path.exists()
    assert capsys.readouterr().out == ' |\r   Downloading illustration...\r \r'
    assert mocked_api.method_calls == [
        call.protected_download('url', tmp_path, tmp_path / 'filename')
    ]



class FakeData:
    def url(self):
        return 'fake'

data = FakeData

def test_download_image_coords(monkeypatch, capsys):
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
