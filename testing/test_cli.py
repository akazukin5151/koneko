import pytest
from unittest.mock import Mock, call

from koneko import cli


@pytest.mark.parametrize("args", (
    ['1', '2232374'],
    ['a', '2232374'],
    ['https://www.pixiv.net/en/users/2232374']))
def test_mode1(monkeypatch, args):
    mock = Mock()
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko'] + args))
    monkeypatch.setattr('koneko.cli.main.ArtistModeLoop', mock)

    args = cli.handle_vh()
    assert cli.launch_mode(args, True)
    assert mock.call_args_list == [call('2232374')]
    assert mock.mock_calls == [call('2232374'), call().start()]

@pytest.mark.parametrize("args", (
    ['2', '78823485'],
    ['i', '78823485'],
    ['https://www.pixiv.net/en/artworks/78823485']))
def test_mode2(monkeypatch, args):
    mock = Mock()
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko'] + args))
    monkeypatch.setattr('koneko.cli.main.ViewPostModeLoop', mock)

    args = cli.handle_vh()
    assert cli.launch_mode(args, True)
    assert mock.call_args_list == [call('78823485')]
    assert mock.mock_calls == [call('78823485'), call().start()]

@pytest.mark.parametrize("args", (['3', '78823485'], ['f', '78823485'], ['3'], ['f']))
def test_mode3(monkeypatch, args):
    mock = Mock()
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko'] + args))
    monkeypatch.setattr('koneko.cli.main.FollowingUserModeLoop', mock)
    monkeypatch.setattr('koneko.utils.ask_your_id', lambda x: '78823485')

    args = cli.handle_vh()
    assert cli.launch_mode(args, '78823485')
    assert mock.call_args_list == [call('78823485')]
    assert mock.mock_calls == [call('78823485'), call().start()]


@pytest.mark.parametrize("args", (['4', 'searchstring'], ['searchstring']))
def test_mode4(monkeypatch, args):
    mock = Mock()
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko'] + args))
    monkeypatch.setattr('koneko.cli.main.SearchUsersModeLoop', mock)

    args = cli.handle_vh()
    assert cli.launch_mode(args, True)
    assert mock.call_args_list == [call('searchstring')]
    assert mock.mock_calls == [call('searchstring'), call().start()]

@pytest.mark.parametrize("mode", ('5', 'n'))
def test_mode5(monkeypatch, mode):
    mock = Mock()
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', mode]))
    monkeypatch.setattr('koneko.cli.main.illust_follow_mode_loop', mock)

    args = cli.handle_vh()
    assert cli.launch_mode(args, True)
    assert mock.call_args_list == [call()]

@pytest.mark.parametrize("arg", ('-v', '--version'))
def test_version(monkeypatch, arg):
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', arg]))
    assert cli.handle_vh() is False

@pytest.mark.parametrize("arg", ('-h', '--help'))
def test_help(monkeypatch, arg):
    monkeypatch.setattr('koneko.cli.docopt',
        lambda x: {'-h': True, '--help': True, '-v': False, '--version': False})
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', arg]))
    assert cli.handle_vh() is False

