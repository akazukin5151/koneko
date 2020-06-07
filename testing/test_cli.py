import pytest

from koneko import cli

# No cli args
# (No longer tested here, as main.py does not call cli if no args)

def test_mode1_link_only(monkeypatch):
    monkeypatch.setattr('koneko.cli.sys.argv',
            (['koneko', 'https://www.pixiv.net/en/users/2232374']))
    assert cli.process_cli_args() == ('1', '2232374')

def test_mode1_mode_and_link(monkeypatch):
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', '1', '2232374']))
    assert cli.process_cli_args() == ('1', '2232374')
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', 'a', '2232374']))
    assert cli.process_cli_args() == ('1', '2232374')

def test_mode2_link_only(monkeypatch):
    monkeypatch.setattr('koneko.cli.sys.argv',
            (['koneko', 'https://www.pixiv.net/en/artworks/78823485']))
    assert cli.process_cli_args() == ('2', '78823485')

def test_mode2_mode_and_link(monkeypatch):
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', '2', '78823485']))
    assert cli.process_cli_args() == ('2', '78823485')
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', 'i', '78823485']))
    assert cli.process_cli_args() == ('2', '78823485')

def test_mode3_mode_and_link(monkeypatch):
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', '3', '78823485']))
    assert cli.process_cli_args() == ('3', '78823485')
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', 'f', '78823485']))
    assert cli.process_cli_args() == ('3', '78823485')

def test_mode3_no_link(monkeypatch):
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', '3']))
    assert cli.process_cli_args() == ('3', None)
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', 'f']))
    assert cli.process_cli_args() == ('3', None)

def test_mode4_string_only(monkeypatch):
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', 'searchstring']))
    assert cli.process_cli_args() == ('4', 'searchstring')

def test_mode4_mode_and_string(monkeypatch):
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', '4', 'searchstring']))
    assert cli.process_cli_args() == ('4', 'searchstring')

def test_mode5_no_link(monkeypatch):
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', '5']))
    assert cli.process_cli_args() == ('5', None)
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', 'n']))
    assert cli.process_cli_args() == ('5', None)
