import os
import sys
import pytest
from pathlib import Path

from koneko import main, utils


@pytest.fixture
def set_config(monkeypatch):
    credentials, your_id = utils.config()
    monkeypatch.setattr('koneko.main.utils.config',
                        lambda: (credentials, your_id))

@pytest.mark.integration
def test_mode1(monkeypatch, set_config):
    """Assume that you have valid working credentials and config stored
    in ~/.config/koneko/config.ini
    And internet connection
    """
    monkeypatch.setattr('koneko.main.cli.process_cli_args',
                        lambda: (False, '1', 2232374))

    # Make the prompt exit (return True will cause main() to loop forever)
    monkeypatch.setattr('koneko.main.prompt.gallery_like_prompt',
                        lambda x: sys.exit(0))

    with pytest.raises(SystemExit):
        main.main()

@pytest.mark.integration
def test_mode2(monkeypatch, set_config):
    """Assume that you have valid working credentials and config stored
    in ~/.config/koneko/config.ini
    And internet connection
    """
    monkeypatch.setattr('koneko.main.cli.process_cli_args',
                        lambda: (False, '2', 78823485))

    monkeypatch.setattr('koneko.main.ui.prompt.image_prompt',
                        lambda x: sys.exit(0))

    with pytest.raises(SystemExit):
        main.main()

@pytest.mark.integration
def test_mode3(monkeypatch, set_config):
    """Assume that you have valid working credentials and config stored
    in ~/.config/koneko/config.ini
    And internet connection
    """
    monkeypatch.setattr('koneko.main.cli.process_cli_args',
                        lambda: (False, '3', None))

    monkeypatch.setattr('koneko.main.prompt.user_prompt',
                        lambda x: sys.exit(0))

    monkeypatch.setattr('builtins.input', lambda x: '')

    with pytest.raises(SystemExit):
        main.main()

@pytest.mark.integration
def test_mode4(monkeypatch, set_config):
    """Assume that you have valid working credentials and config stored
    in ~/.config/koneko/config.ini
    And internet connection
    """
    monkeypatch.setattr('koneko.main.cli.process_cli_args',
                        lambda: (False, '4', 'gomzi'))

    monkeypatch.setattr('koneko.main.prompt.user_prompt',
                        lambda x: sys.exit(0))

    with pytest.raises(SystemExit):
        main.main()

@pytest.mark.integration
def test_mode5(monkeypatch, set_config):
    """Assume that you have valid working credentials and config stored
    in ~/.config/koneko/config.ini
    And internet connection
    """
    monkeypatch.setattr('koneko.main.cli.process_cli_args',
                        lambda: (False, '5', None))

    monkeypatch.setattr('koneko.main.prompt.gallery_like_prompt',
                        lambda x: sys.exit(0))

    with pytest.raises(SystemExit):
        main.main()
