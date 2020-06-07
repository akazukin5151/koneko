import pytest

from koneko import cli

def test_cli(monkeypatch):
    # No cli args
    # (No longer tested here, as main.py does not call cli if no args)

    # No mode specified
    ## Mode 1, link only
    monkeypatch.setattr('koneko.cli.sys.argv',
            (['koneko', 'https://www.pixiv.net/en/users/2232374']))
    assert cli.process_cli_args() == ('1', '2232374')

    ## Mode 2, link only
    monkeypatch.setattr('koneko.cli.sys.argv',
            (['koneko', 'https://www.pixiv.net/en/artworks/78823485']))
    assert cli.process_cli_args() == ('2', '78823485')

    ## Mode 4
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', 'searchstring']))
    assert cli.process_cli_args() == ('4', 'searchstring')


    # Mode specified
    ## Mode 1, ID only
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', '1', '2232374']))
    assert cli.process_cli_args() == ('1', '2232374')
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', 'a', '2232374']))
    assert cli.process_cli_args() == ('1', '2232374')

    ## Mode 2, ID only
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', '2', '78823485']))
    assert cli.process_cli_args() == ('2', '78823485')
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', 'i', '78823485']))
    assert cli.process_cli_args() == ('2', '78823485')

    ## Mode 3, ID given
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', '3', '78823485']))
    assert cli.process_cli_args() == ('3', '78823485')
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', 'f', '78823485']))
    assert cli.process_cli_args() == ('3', '78823485')

    ## Mode 3
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', '3']))
    assert cli.process_cli_args() == ('3', None)
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', 'f']))
    assert cli.process_cli_args() == ('3', None)

    ## Mode 4
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', '4', 'searchstring']))
    assert cli.process_cli_args() == ('4', 'searchstring')

    ## Mode 5
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', '5']))
    assert cli.process_cli_args() == ('5', None)
    monkeypatch.setattr('koneko.cli.sys.argv', (['koneko', 'n']))
    assert cli.process_cli_args() == ('5', None)
