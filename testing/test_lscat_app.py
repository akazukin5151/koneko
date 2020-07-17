from unittest.mock import Mock, call

import pytest

from koneko.lscat_app import FakeData
from koneko import lscat_app, lscat, KONEKODIR
from conftest import CustomExit, raises_customexit


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
    monkeypatch.setattr('koneko.lscat_prompt.gallery_user_loop', mock)

    monkeypatch.setattr('koneko.picker.lscat_app_main', lambda: 3)
    monkeypatch.setattr('koneko.picker.pick_dir', lambda: tmp_path)

    lscat_app.main()
    assert mock.mock_calls == mock.call_args_list == [
        call(
            FakeData(tmp_path),
            lscat.TrackDownloads,
        )
    ]


@pytest.mark.parametrize('argv', (['4'], []))
def test_browse_cache_invis(monkeypatch, tmp_path, argv):
    mock = Mock()
    monkeypatch.setattr('koneko.lscat_app.sys.argv', [True] + argv)
    monkeypatch.setattr('koneko.lscat_prompt.gallery_user_loop', mock)

    monkeypatch.setattr('koneko.picker.lscat_app_main', lambda: 3)
    monkeypatch.setattr('koneko.picker.pick_dir', lambda: tmp_path)
    (tmp_path / '.koneko').touch()

    lscat_app.main()
    assert mock.mock_calls == mock.call_args_list == [
        call(
            FakeData(tmp_path),
            lscat.TrackDownloadsUsers,
        )
    ]


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
        )
    ]



def test_maybe_ask_assistant(monkeypatch):
    assert lscat_app.maybe_ask_assistant([str(x) for x in range(5)]) == list(range(5))

    monkeypatch.setattr('koneko.picker.ask_assistant', lambda: list(range(3)))
    assert lscat_app.maybe_ask_assistant(None) == list(range(3))


def test_maybe_thumbnail_size(monkeypatch, send_enter, capsys):
    monkeypatch.setattr('koneko.assistants.thumbnail_size_assistant', lambda: 99)

    lscat_app.config_assistance(['1'])

    captured = capsys.readouterr()
    assert captured.out == "\n\nYour recommended settings are:\nimage_thumbnail_size = 99\n"


def test_maybe_xpadding_img_width(monkeypatch, send_enter, capsys):
    monkeypatch.setattr('koneko.assistants.xpadding_assistant', lambda x: (99, 999))

    lscat_app.config_assistance(['2'])

    captured = capsys.readouterr()
    assert captured.out == '\n\nYour recommended settings are:\nimage_width = 999\nimages_x_spacing = 99\n'


def test_maybe_ypadding_img_height(monkeypatch, send_enter, capsys):
    monkeypatch.setattr('koneko.assistants.ypadding_assistant', lambda x: (99, 999))

    lscat_app.config_assistance(['3'])

    captured = capsys.readouterr()
    assert captured.out == '\n\nYour recommended settings are:\nimage_height = 999\nimages_y_spacing = 99\n'


def test_maybe_page_spacing(monkeypatch, send_enter, capsys):
    monkeypatch.setattr('koneko.assistants.page_spacing_assistant', lambda x: 99)

    lscat_app.config_assistance(['4'])

    captured = capsys.readouterr()
    assert captured.out == '\n\nYour recommended settings are:\npage_spacing = 99\n'


def test_maybe_print_spacing(monkeypatch, send_enter, capsys):
    monkeypatch.setattr('koneko.assistants.gallery_print_spacing_assistant', lambda *a: range(5))

    lscat_app.config_assistance(['5'])

    captured = capsys.readouterr()
    assert captured.out == '\n\nYour recommended settings are:\ngallery_print_spacing = 0,1,2,3,4\n'


def test_maybe_print_xcoord(monkeypatch, send_enter, capsys):
    monkeypatch.setattr('koneko.assistants.user_info_assistant', lambda *a: 99)

    lscat_app.config_assistance(['6'])

    captured = capsys.readouterr()
    assert captured.out == '\n\nYour recommended settings are:\nusers_print_name_xcoord = 99\n'



@pytest.mark.parametrize('argv', ([], ['1']))
def test_config_assistant_1(monkeypatch, capsys, send_enter, argv):
    monkeypatch.setattr('koneko.lscat_app.sys.argv', [True, '1'] + argv)
    monkeypatch.setattr('koneko.picker.ask_assistant', lambda: [1])
    monkeypatch.setattr('koneko.assistants.thumbnail_size_assistant', lambda: 99)

    lscat_app.main()

    captured = capsys.readouterr()
    assert captured.out == '\n\nYour recommended settings are:\nimage_thumbnail_size = 99\n'


@pytest.mark.parametrize('argv', ([], ['2']))
def test_config_assistant_2(monkeypatch, capsys, send_enter, argv):
    monkeypatch.setattr('koneko.lscat_app.sys.argv', [True, '1'] + argv)
    monkeypatch.setattr('koneko.picker.ask_assistant', lambda: [2])
    monkeypatch.setattr('koneko.assistants.xpadding_assistant', lambda x: (99, 999))

    lscat_app.main()

    captured = capsys.readouterr()
    assert captured.out == '\n\nYour recommended settings are:\nimage_width = 999\nimages_x_spacing = 99\n'



@pytest.mark.parametrize('argv', ([], ['3']))
def test_config_assistant_3(monkeypatch, capsys, send_enter, argv):
    monkeypatch.setattr('koneko.lscat_app.sys.argv', [True, '1'] + argv)
    monkeypatch.setattr('koneko.picker.ask_assistant', lambda: [3])
    monkeypatch.setattr('koneko.assistants.ypadding_assistant', lambda x: (99, 999))

    lscat_app.main()

    captured = capsys.readouterr()
    assert captured.out == '\n\nYour recommended settings are:\nimage_height = 999\nimages_y_spacing = 99\n'


@pytest.mark.parametrize('argv', ([], ['4']))
def test_config_assistant_4(monkeypatch, capsys, send_enter, argv):
    monkeypatch.setattr('koneko.lscat_app.sys.argv', [True, '1'] + argv)
    monkeypatch.setattr('koneko.picker.ask_assistant', lambda: [4])
    monkeypatch.setattr('koneko.assistants.page_spacing_assistant', lambda x: 99)

    lscat_app.main()

    captured = capsys.readouterr()
    assert captured.out == '\n\nYour recommended settings are:\npage_spacing = 99\n'


@pytest.mark.parametrize('argv', ([], ['5']))
def test_config_assistant_5(monkeypatch, capsys, send_enter, argv):
    monkeypatch.setattr('koneko.lscat_app.sys.argv', [True, '1'] + argv)
    monkeypatch.setattr('koneko.picker.ask_assistant', lambda: [5])
    monkeypatch.setattr('koneko.assistants.gallery_print_spacing_assistant', lambda *a: range(5))

    lscat_app.main()

    captured = capsys.readouterr()
    assert captured.out == '\n\nYour recommended settings are:\ngallery_print_spacing = 0,1,2,3,4\n'


@pytest.mark.parametrize('argv', ([], ['6']))
def test_config_assistant_6(monkeypatch, capsys, send_enter, argv):
    monkeypatch.setattr('koneko.lscat_app.sys.argv', [True, '1'] + argv)
    monkeypatch.setattr('koneko.picker.ask_assistant', lambda: [6])
    monkeypatch.setattr('koneko.assistants.user_info_assistant', lambda *a: 99)

    lscat_app.main()

    captured = capsys.readouterr()
    assert captured.out == '\n\nYour recommended settings are:\nusers_print_name_xcoord = 99\n'
