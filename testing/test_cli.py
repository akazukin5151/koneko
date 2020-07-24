from unittest.mock import Mock, call

import pytest

from koneko import cli


def capture_logging_in(capsys):
    captured = capsys.readouterr()
    assert captured.out == 'Logging in...\n'


@pytest.mark.parametrize('args', (
    ['1', '2232374'],
    ['a', '2232374'],
    ['https://www.pixiv.net/en/users/2232374']))
def test_mode1(monkeypatch, args, capsys):
    mock = Mock()
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko'] + args))
    monkeypatch.setattr('koneko.cli.main.ArtistModeLoop', mock)

    args = cli.handle_vh()
    assert cli.launch_mode(args, True)
    assert mock.call_args_list == [call('2232374')]
    assert mock.mock_calls == [call('2232374'), call().start()]
    capture_logging_in(capsys)

@pytest.mark.parametrize('args', (
    ['2', '78823485'],
    ['i', '78823485'],
    ['https://www.pixiv.net/en/artworks/78823485']))
def test_mode2(monkeypatch, args, capsys):
    mock = Mock()
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko'] + args))
    monkeypatch.setattr('koneko.cli.main.ViewPostModeLoop', mock)

    args = cli.handle_vh()
    assert cli.launch_mode(args, True)
    assert mock.call_args_list == [call('78823485')]
    assert mock.mock_calls == [call('78823485'), call().start()]
    capture_logging_in(capsys)

@pytest.mark.parametrize('args', (['3', '78823485'], ['f', '78823485']))
def test_mode3_mode_and_input(monkeypatch, args, capsys):
    mock = Mock()
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko'] + args))
    monkeypatch.setattr('koneko.cli.main.FollowingUserModeLoop', mock)
    monkeypatch.setattr('koneko.main.ask_your_id', lambda x: '78823485')

    args = cli.handle_vh()
    assert cli.launch_mode(args, '78823485')
    assert mock.call_args_list == [call('78823485')]
    assert mock.mock_calls == [call('78823485'), call().start()]
    capture_logging_in(capsys)

@pytest.mark.parametrize('args', (['3'], ['f']))
def test_mode3_no_input(monkeypatch, args, capsys):
    mock = Mock()
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko'] + args))
    monkeypatch.setattr('koneko.cli.main.FollowingUserModeLoop', mock)
    monkeypatch.setattr('koneko.main.ask_your_id', lambda x: '78823485')

    args = cli.handle_vh()
    assert cli.launch_mode(args, '78823485')
    assert mock.call_args_list == [call('78823485')]
    assert mock.mock_calls == [call('78823485'), call().start('78823485')]
    capture_logging_in(capsys)


@pytest.mark.parametrize('args', (['4', 'searchstring'], ['searchstring']))
def test_mode4(monkeypatch, args):
    mock = Mock()
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko'] + args))
    monkeypatch.setattr('koneko.cli.main.SearchUsersModeLoop', mock)

    args = cli.handle_vh()
    assert cli.launch_mode(args, True)
    assert mock.call_args_list == [call('searchstring')]
    assert mock.mock_calls == [call('searchstring'), call().start()]

@pytest.mark.parametrize('mode', ('5', 'n'))
def test_mode5(monkeypatch, mode):
    mock = Mock()
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', mode]))
    monkeypatch.setattr('koneko.cli.main.illust_follow_mode', mock)

    args = cli.handle_vh()
    assert cli.launch_mode(args, True)
    assert mock.call_args_list == [call()]


@pytest.mark.parametrize('mode', ('6', 'r'))
def test_mode6(monkeypatch, mode):
    mock = Mock()
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', mode]))
    monkeypatch.setattr('koneko.cli.main.illust_recommended_mode', mock)

    args = cli.handle_vh()
    assert cli.launch_mode(args, True)
    assert mock.call_args_list == [call()]


def test_frequent(monkeypatch):
    mock = Mock()
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', 'q']))
    monkeypatch.setattr('koneko.cli.main.frequent', mock)

    args = cli.handle_vh()
    assert cli.launch_mode(args, True)
    assert mock.call_args_list == [call()]


@pytest.mark.parametrize('arg', ('-v', '--version'))
def test_version(monkeypatch, arg):
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', arg]))
    assert cli.handle_vh() is False

@pytest.mark.parametrize('arg', ('-h', '--help'))
def test_help(monkeypatch, arg, capsys):
    monkeypatch.setattr('koneko.cli.docopt',
        lambda x: {'-h': True, '--help': True, '-v': False, '--version': False})
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', arg]))
    assert cli.handle_vh() is False

    captured = capsys.readouterr()
    assert captured.out == cli.__doc__ + '\n'
