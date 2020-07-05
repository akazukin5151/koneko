import pytest
from pathlib import Path
from unittest.mock import Mock, call

from koneko import lscat_app, lscat, KONEKODIR
from koneko.lscat_app import FakeData


@pytest.mark.parametrize('argv', (['2'], []))
def test_display_gallery(monkeypatch, argv):
    mock = Mock()
    monkeypatch.setattr('koneko.lscat.show_instant', mock)
    monkeypatch.setattr('koneko.lscat_app.sys.argv', [True] + argv)

    monkeypatch.setattr('koneko.picker.lscat_app_main', lambda: 1)

    lscat_app.main()
    assert mock.mock_calls == mock.call_args_list == [
        call(
            lscat.TrackDownloads,
            FakeData(KONEKODIR / 'testgallery'),
            True
        )
    ]


@pytest.mark.parametrize('argv', (['3'], []))
def test_display_user(monkeypatch, argv):
    mock = Mock()
    monkeypatch.setattr('koneko.lscat_app.sys.argv', [True] + argv)
    monkeypatch.setattr('koneko.lscat.show_instant', mock)

    monkeypatch.setattr('koneko.picker.lscat_app_main', lambda: 2)

    lscat_app.main()
    assert mock.mock_calls == mock.call_args_list == [
        call(
            lscat.TrackDownloadsUsers,
            FakeData(KONEKODIR / 'testuser')
        )
    ]


@pytest.mark.parametrize('argv', (['4'], []))
def test_browse_cache_noinvis(monkeypatch, tmp_path, argv):
    mock = Mock()
    monkeypatch.setattr('koneko.lscat_app.sys.argv', [True] + argv)
    monkeypatch.setattr('koneko.lscat.show_instant', mock)

    monkeypatch.setattr('koneko.picker.lscat_app_main', lambda: 3)
    monkeypatch.setattr('koneko.picker.pick_dir', lambda: tmp_path)

    lscat_app.main()
    assert mock.mock_calls == mock.call_args_list == [
        call(
            lscat.TrackDownloads,
            FakeData(tmp_path),
            True
        )
    ]


@pytest.mark.parametrize('argv', (['4'], []))
def test_browse_cache_invis(monkeypatch, tmp_path, argv):
    mock = Mock()
    monkeypatch.setattr('koneko.lscat_app.sys.argv', [True] + argv)
    monkeypatch.setattr('koneko.lscat.show_instant', mock)

    monkeypatch.setattr('koneko.picker.lscat_app_main', lambda: 3)
    monkeypatch.setattr('koneko.picker.pick_dir', lambda: tmp_path)
    (tmp_path / '.koneko').touch()

    lscat_app.main()
    assert mock.mock_calls == mock.call_args_list == [
        call(
            lscat.TrackDownloadsUsers,
            FakeData(tmp_path)
        )
    ]


# TODO move common to conftest
class CustomExit(SystemExit): pass

def raises_customexit(*a, **k): raise CustomExit()


def test_display_path_cli_invalid_quits(monkeypatch):
    mock = Mock()
    monkeypatch.setattr('koneko.lscat_app.sys.argv', [True, '5', 'notapath'])
    monkeypatch.setattr('koneko.picker.lscat_app_main', lambda: 4)
    monkeypatch.setattr('koneko.lscat.show_instant', mock)
    monkeypatch.setattr('koneko.lscat_app.sys.exit', raises_customexit)

    with pytest.raises(CustomExit):
        lscat_app.main()


def test_display_path_cli_complete(monkeypatch, tmp_path):
    mock = Mock()
    monkeypatch.setattr('koneko.lscat_app.sys.argv', [True, '5', str(tmp_path)])
    monkeypatch.setattr('koneko.lscat.show_instant', mock)

    monkeypatch.setattr('koneko.picker.lscat_app_main', lambda: 4)

    lscat_app.main()
    assert mock.mock_calls == mock.call_args_list == [
        call(
            lscat.TrackDownloads,
            FakeData(str(tmp_path)),
            True
        )
    ]


def test_display_path_cli_incomplete(monkeypatch, tmp_path):
    mock = Mock()
    monkeypatch.setattr('koneko.lscat_app.sys.argv', [True, '5'])
    monkeypatch.setattr('koneko.lscat.show_instant', mock)

    monkeypatch.setattr('koneko.picker.lscat_app_main', lambda: 4)
    responses = iter((str(tmp_path),))
    monkeypatch.setattr('builtins.input', lambda x=None: next(responses))

    lscat_app.main()
    assert mock.mock_calls == mock.call_args_list == [
        call(
            lscat.TrackDownloads,
            FakeData(str(tmp_path)),
            True
        )
    ]





