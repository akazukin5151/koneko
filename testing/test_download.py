from pathlib import Path
from collections import namedtuple

import pytest

from koneko import download


def test_save_number_of_artists(tmp_path):
    FakeData = namedtuple('data', ('download_path', 'splitpoint'))
    data = FakeData(tmp_path, 30)
    download.save_number_of_artists(data)
    with open(tmp_path / '.koneko', 'r') as f:
        assert f.read() == '30'


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
