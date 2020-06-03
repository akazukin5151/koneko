"""Kind of pointless; just to test the individual functions don't crash
(And to bump up the coverage)
"""

import os
import sys
import pytest
from pathlib import Path

from koneko import main, utils, ui


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

@pytest.mark.integration
def test_open_link(monkeypatch):
    monkeypatch.setattr('koneko.ui.ArtistGallery.start', lambda x: True)
    monkeypatch.setattr('koneko.ui.os.system', lambda x: True)
    mode = ui.ArtistGallery(1234)
    mode.data.image_id = lambda x: 5678
    mode.open_link_coords(1, 2)

@pytest.mark.integration
def test_download_image(monkeypatch):
    monkeypatch.setattr('koneko.ui.ArtistGallery.start', lambda x: True)
    monkeypatch.setattr('koneko.ui.download.download_image_verified', lambda **x: True)
    mode = ui.ArtistGallery(1234)
    mode.data.post_json = lambda x: 5678
    mode.download_image_coords(1, 2)


