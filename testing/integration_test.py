"""Kind of pointless; just to test the individual functions don't crash
(And to bump up the coverage)

Assume that you have valid working credentials and config stored
in ~/.config/koneko/config.ini
And internet connection
"""
import sys

import pytest

from koneko import main, utils

from conftest import CustomExit, raises_customexit


@pytest.fixture
def set_argc_to_one(monkeypatch):
    monkeypatch.setattr('koneko.main.sys.argv', [1])


def cli_core(monkeypatch, args: tuple, prompt: str):
    monkeypatch.setattr('koneko.main.AbstractLoop._save_history', lambda x: True)
    monkeypatch.setattr('koneko.main.sys.argv', [None, *args])
    monkeypatch.setattr(prompt, raises_customexit)
    with pytest.raises(CustomExit):
        main.main()

@pytest.mark.integration
def test_mode1_cli(monkeypatch):
    cli_core(monkeypatch, ('1', '2232374'), 'koneko.main.prompt.gallery_like_prompt')

@pytest.mark.integration
def test_mode2_cli(monkeypatch):
    cli_core(monkeypatch, ('2', '78823485'), 'koneko.main.prompt.image_prompt')

@pytest.mark.integration
def test_mode3_cli(monkeypatch, send_enter):
    cli_core(monkeypatch, ('3',), 'koneko.main.prompt.user_prompt')

@pytest.mark.integration
def test_mode4_cli(monkeypatch):
    cli_core(monkeypatch, ('4', 'gomzi'), 'koneko.main.prompt.user_prompt')

@pytest.mark.integration
def test_mode5_cli(monkeypatch):
    cli_core(monkeypatch, ('5',), 'koneko.main.prompt.gallery_like_prompt')



def input_core(monkeypatch, responses: iter, prompt: str):
    monkeypatch.setattr('koneko.main.AbstractLoop._save_history', lambda x: True)
    monkeypatch.setattr('builtins.input', lambda x=None: next(responses))
    monkeypatch.setattr(prompt, raises_customexit)
    with pytest.raises(CustomExit):
        main.main()

@pytest.mark.integration
def test_mode1_input(monkeypatch, set_argc_to_one):
    responses = iter(['1', 'https://www.pixiv.net/en/users/2232374'])
    input_core(monkeypatch, responses, 'koneko.main.prompt.gallery_like_prompt')

@pytest.mark.integration
def test_mode2_input(monkeypatch, set_argc_to_one):
    responses = iter(['2', 'https://www.pixiv.net/en/artworks/78823485'])
    input_core(monkeypatch, responses, 'koneko.main.ui.prompt.image_prompt')

@pytest.mark.integration
def test_mode3_input(monkeypatch, set_argc_to_one):
    responses = iter(['3', ''])
    input_core(monkeypatch, responses, 'koneko.main.prompt.user_prompt')

@pytest.mark.integration
def test_mode4_input(monkeypatch, set_argc_to_one):
    responses = iter(['4', 'gomzi'])
    input_core(monkeypatch, responses, 'koneko.main.prompt.user_prompt')

@pytest.mark.integration
def test_mode5_input(monkeypatch, set_argc_to_one):
    responses = iter(['5'])
    input_core(monkeypatch, responses, 'koneko.main.prompt.gallery_like_prompt')



@pytest.mark.integration
def test_open_link(monkeypatch):
    monkeypatch.setattr('koneko.ui.ArtistGallery.start', lambda x: True)
    monkeypatch.setattr('koneko.ui.os.system', lambda x: True)

    class FakeData:
        def image_id(self):
            return 1
    data = FakeData

    utils.open_link_coords(data, 1, 2)

#@pytest.mark.integration
#def test_download_image(monkeypatch):
#    monkeypatch.setattr('koneko.ui.ArtistGallery.start', lambda x: True)
#    monkeypatch.setattr('koneko.ui.download.download_url_verified', lambda **x: True)
#    mode = ui.ArtistGallery(1234)
#    mode.data.post_json = lambda x: 5678
#    mode.download_image_coords(1, 2)
