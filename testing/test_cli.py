import pytest
from unittest.mock import Mock, call

from koneko import cli

def test_mode1_link_only(monkeypatch):
    mock = Mock()
    monkeypatch.setattr('koneko.cli.sys.argv',
                        (['koneko', 'https://www.pixiv.net/en/users/2232374']))
    monkeypatch.setattr('koneko.cli.main.ArtistModeLoop', mock)

    args = cli.handle_vh()
    assert cli.launch_mode(args, True)
    assert mock.call_args_list == [call('2232374')]
    assert mock.mock_calls == [call('2232374'), call().start()]

@pytest.mark.parametrize("mode", ('1', 'a'))
def test_mode1_mode_and_link(monkeypatch, mode):
    mock = Mock()
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', mode, '2232374']))
    monkeypatch.setattr('koneko.cli.main.ArtistModeLoop', mock)

    args = cli.handle_vh()
    assert cli.launch_mode(args, True)
    assert mock.call_args_list == [call('2232374')]
    assert mock.mock_calls == [call('2232374'), call().start()]

def test_mode2_link_only(monkeypatch):
    mock = Mock()
    monkeypatch.setattr('koneko.cli.sys.argv',
                        (['koneko', 'https://www.pixiv.net/en/artworks/78823485']))
    monkeypatch.setattr('koneko.cli.main.ViewPostModeLoop', mock)

    args = cli.handle_vh()
    assert cli.launch_mode(args, True)
    assert mock.call_args_list == [call('78823485')]
    assert mock.mock_calls == [call('78823485'), call().start()]

@pytest.mark.parametrize("mode", ('2', 'i'))
def test_mode2_mode_and_link(monkeypatch, mode):
    mock = Mock()
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', mode, '78823485']))
    monkeypatch.setattr('koneko.cli.main.ViewPostModeLoop', mock)

    args = cli.handle_vh()
    assert cli.launch_mode(args, True)
    assert mock.call_args_list == [call('78823485')]
    assert mock.mock_calls == [call('78823485'), call().start()]

@pytest.mark.parametrize("mode", ('3', 'f'))
def test_mode3_mode_and_link(monkeypatch, mode):
    mock = Mock()
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', mode, '78823485']))
    monkeypatch.setattr('koneko.cli.main.FollowingUserModeLoop', mock)

    args = cli.handle_vh()
    assert cli.launch_mode(args, True)
    assert mock.call_args_list == [call('78823485')]
    assert mock.mock_calls == [call('78823485'), call().start()]

@pytest.mark.parametrize("mode", ('3', 'f'))
def test_mode3_no_link(monkeypatch, mode):
    mock = Mock()
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', mode]))
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
def test_mode5_no_link(monkeypatch, mode):
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

