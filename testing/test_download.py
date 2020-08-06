from pathlib import Path

import pytest

from koneko import download


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
