import pytest
from unittest.mock import Mock, call

from koneko import cli

def test_mode1_link_only(monkeypatch):
    monkeypatch.setattr('koneko.cli.sys.argv',
                        (['koneko', 'https://www.pixiv.net/en/users/2232374']))
    mock = Mock()
    monkeypatch.setattr('koneko.cli.main.ArtistModeLoop', mock)
    args = cli.handle_vh()
    assert cli.launch_mode(args, True)
    assert mock.call_count == 1
    assert mock.call_args_list == [call('2232374')]
    assert mock.mock_calls == [call('2232374'), call().start()]

@pytest.mark.parametrize("mode", ('1', 'a'))
def test_mode1_mode_and_link(monkeypatch, mode):
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', mode, '2232374']))
    mock = Mock()
    monkeypatch.setattr('koneko.cli.main.ArtistModeLoop', mock)
    args = cli.handle_vh()
    assert cli.launch_mode(args, True)
    assert mock.call_count == 1
    assert mock.call_args_list == [call('2232374')]
    assert mock.mock_calls == [call('2232374'), call().start()]

#def test_mode2_link_only(monkeypatch):
#    monkeypatch.setattr('koneko.cli.sys.argv',
#                        (['koneko', 'https://www.pixiv.net/en/artworks/78823485']))
#    assert cli.process_cli_args() == ('2', '78823485')
#
#def test_mode2_mode_and_link(monkeypatch):
#    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', '2', '78823485']))
#    assert cli.process_cli_args() == ('2', '78823485')
#    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', 'i', '78823485']))
#    assert cli.process_cli_args() == ('2', '78823485')
#
#def test_mode3_mode_and_link(monkeypatch):
#    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', '3', '78823485']))
#    assert cli.process_cli_args() == ('3', '78823485')
#    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', 'f', '78823485']))
#    assert cli.process_cli_args() == ('3', '78823485')
#
#def test_mode3_no_link(monkeypatch):
#    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', '3']))
#    assert cli.process_cli_args() == ('3', '')
#    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', 'f']))
#    assert cli.process_cli_args() == ('3', '')
#
#def test_mode4_string_only(monkeypatch):
#    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', 'searchstring']))
#    assert cli.process_cli_args() == ('4', 'searchstring')
#
#def test_mode4_mode_and_string(monkeypatch):
#    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', '4', 'searchstring']))
#    assert cli.process_cli_args() == ('4', 'searchstring')
#
#def test_mode5_no_link(monkeypatch):
#    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', '5']))
#    assert cli.process_cli_args() == ('5', '')
#    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', 'n']))
#    assert cli.process_cli_args() == ('5', '')
#
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

