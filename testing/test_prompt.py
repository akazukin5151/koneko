from collections import namedtuple
from abc import ABC, abstractmethod
from unittest.mock import Mock, call

import pytest
from blessed.keyboard import Keystroke

from koneko import prompt
from conftest import CustomExit, raises_customexit


@pytest.fixture
def customexit_to_quit(monkeypatch):
    monkeypatch.setattr('koneko.prompt.sys.exit', raises_customexit)

def ask_quit_capture(capsys):
    captured = capsys.readouterr()
    assert captured.out == '\nAre you sure you want to exit?\n'

def test_ask_quit_do_nothing(monkeypatch, patch_cbreak, customexit_to_quit, capsys):
    # Does not call the 'class'
    fake_inkey = namedtuple('FakeInKey', ('code',), defaults=(True,))
    monkeypatch.setattr('koneko.prompt.TERM.inkey', fake_inkey)
    assert not prompt.ask_quit()
    ask_quit_capture(capsys)


def test_ask_quit_enter(monkeypatch, patch_cbreak, customexit_to_quit, capsys):
    fake_inkey = namedtuple('FakeInKey', ('code',), defaults=(343,))
    monkeypatch.setattr('koneko.prompt.TERM.inkey', fake_inkey)
    with pytest.raises(CustomExit):
        assert prompt.ask_quit()
    ask_quit_capture(capsys)


@pytest.mark.parametrize('letter', ['y', 'q', '', 'h'])
def test_ask_quit_letter(monkeypatch, patch_cbreak, customexit_to_quit, letter, capsys):
    fake_inkey = namedtuple('FakeInKey', ('code',), defaults=(343,))
    fake_inkey.__call__ = lambda x: letter
    monkeypatch.setattr('koneko.prompt.TERM.inkey', fake_inkey)
    with pytest.raises(CustomExit):
        assert prompt.ask_quit()
    ask_quit_capture(capsys)


class FakeInKey(ABC):
    """Fakes the Keystroke constructor in blessed. See:
    https://github.com/jquast/blessed/blob/83748bd9e97f9f2fd849588e677d541e162e3fc6/tests/test_keyboard.py#L98
    """
    def __init__(self):
        self.code = True

    @abstractmethod
    def __call__(self):
        raise NotImplementedError


exit_mock = Mock(side_effect=CustomExit)
fakegallery = Mock()
fakegallery.help = exit_mock
fakegallery.next_page = exit_mock
fakegallery.previous_page = exit_mock
fakegallery.handle_prompt = exit_mock
fakegallery.view_image  = exit_mock


@pytest.mark.parametrize('letter', ['n', 'h', 'b', 'r'])
def test_gallery_like_prompt(monkeypatch, patch_cbreak, letter, capsys):
    class FakeInKeyNew(FakeInKey):
        def __call__(self):
            return Keystroke(ucs=letter, code=1, name=letter)

    fake_inkey = FakeInKeyNew()
    monkeypatch.setattr('koneko.prompt.TERM.inkey', fake_inkey)

    with pytest.raises(CustomExit):
        assert prompt.gallery_like_prompt(fakegallery)

    captured = capsys.readouterr()
    assert captured.out == f'Enter a gallery view command:\n{letter}'


def test_gallery_like_prompt_previous(monkeypatch, patch_cbreak, capsys):
    class FakeInKeyPrev(FakeInKey):
        def __call__(self):
            return Keystroke(ucs='p', code=1, name='p')

    fake_inkey = FakeInKeyPrev()
    monkeypatch.setattr('koneko.prompt.TERM.inkey', fake_inkey)
    monkeypatch.setattr('koneko.ui.AbstractGallery.previous_page', raises_customexit)

    with pytest.raises(CustomExit):
        assert prompt.gallery_like_prompt(fakegallery)

    captured = capsys.readouterr()
    assert captured.out == f'Enter a gallery view command:\np'


def test_gallery_like_prompt_ask_quit(monkeypatch, patch_cbreak, capsys):
    class FakeInKeyQuit(FakeInKey):
        def __call__(self):
            return Keystroke(ucs='q', code=1, name='quit')

    fake_inkey = FakeInKeyQuit()
    monkeypatch.setattr('koneko.prompt.TERM.inkey', fake_inkey)
    monkeypatch.setattr('koneko.prompt.ask_quit', raises_customexit)
    with pytest.raises(CustomExit):
        assert prompt.gallery_like_prompt(fakegallery)

    captured = capsys.readouterr()
    assert captured.out == f'Enter a gallery view command:\nq'


def test_gallery_like_prompt_digits_seq(monkeypatch, patch_cbreak, capsys):
    class FakeInKey1(FakeInKey):
        def __call__(self):
            return Keystroke(ucs='1', code=1, name='1')

    fake_inkey = iter([FakeInKey1()])
    monkeypatch.setattr('koneko.prompt.TERM.inkey', next(fake_inkey))
    with pytest.raises(CustomExit):
        assert prompt.gallery_like_prompt(fakegallery)

    captured = capsys.readouterr()
    assert captured.out == f'Enter a gallery view command:\n11'

# Doesn't work for some reason, but sequencable_keys did trigger
#def test_gallery_like_prompt_3_seq(monkeypatch, patch_cbreak):
#    class FakeInKey0(FakeInKey):
#        def __call__(self):
#            return Keystroke(ucs='o', code=1, name='o')
#
#    class FakeInKey1(FakeInKey):
#        def __call__(self):
#            return Keystroke(ucs='1', code=1, name='1')
#
#    class FakeInKey2(FakeInKey):
#        def __call__(self):
#            return Keystroke(ucs='2', code=1, name='2')
#
#    fake_inkey = iter([FakeInKey0(), FakeInKey1(), FakeInKey2()])
#    monkeypatch.setattr('koneko.prompt.TERM.inkey', next(fake_inkey))
#    monkeypatch.setattr('koneko.utils.open_link_coords', lambda *a: sys.exit(0))
#    fakegallery = FakeGallery()
#    with pytest.raises(SystemExit):
#        assert prompt.gallery_like_prompt(fakegallery)


fakeimage = Mock()
fakeimage.open_image = exit_mock
fakeimage.download_image  = exit_mock
fakeimage.next_image = exit_mock
fakeimage.previous_image = exit_mock
fakeimage.show_full_res= exit_mock
fakeimage.leave = exit_mock

@pytest.mark.parametrize('letter', ('a', 'b', 'q', 'o', 'd', 'n', 'p', 'f'))
def test_image_prompt(monkeypatch, patch_cbreak, letter, capsys):
    monkeypatch.setattr('koneko.prompt.ask_quit', raises_customexit)
    class FakeInKeyNew(FakeInKey):
        def __call__(self):
            return Keystroke(ucs=letter, code=1, name=letter)

    fake_inkey = FakeInKeyNew()
    monkeypatch.setattr('koneko.prompt.TERM.inkey', fake_inkey)
    with pytest.raises(CustomExit):
        assert prompt.image_prompt(fakeimage)

    captured = capsys.readouterr()
    assert captured.out == f'Enter an image view command:\n{letter}'

# Again, multiple key seqs doesn't work
#def test_image_prompt_seq(monkeypatch, patch_cbreak):
#    class FakeInKey1(FakeInKey):
#        def __call__(self):
#            return Keystroke(ucs='1', code=1, name='1')
#
#    class FakeInKey2(FakeInKey):
#        def __call__(self):
#            return Keystroke(ucs='2', code=1, name='2')
#
#    fake_inkey = iter([FakeInKey1(), FakeInKey2()])
#    monkeypatch.setattr('koneko.prompt.TERM.inkey', next(fake_inkey))
#    monkeypatch.setattr('koneko.ui.Image.jump_to_image', raises_customexit)
#    with pytest.raises(CustomExit):
#        assert prompt.image_prompt(fakeimage)



fakeuser = Mock()
fakeuser.next_page = exit_mock
fakeuser.previous_page = exit_mock
fakeuser.reload = exit_mock
fakeuser.go_artist_mode = exit_mock

@pytest.mark.parametrize('letter', ('n', 'r', 'p', 'q'))
def test_user_prompt(monkeypatch, patch_cbreak, letter, capsys):
    monkeypatch.setattr('koneko.prompt.ask_quit', raises_customexit)
    monkeypatch.setattr('koneko.ui.AbstractUsers.previous_page', raises_customexit)
    class FakeInKeyNew(FakeInKey):
        def __call__(self):
            return Keystroke(ucs=letter, code=1, name=letter)

    fake_inkey = FakeInKeyNew()
    monkeypatch.setattr('koneko.prompt.TERM.inkey', fake_inkey)
    with pytest.raises(CustomExit):
        assert prompt.user_prompt(fakeuser)

    captured = capsys.readouterr()
    assert captured.out == f'Enter a user view command:\n{letter}'


def test_user_prompt_seq(monkeypatch, patch_cbreak, capsys):
    class FakeInKey1(FakeInKey):
        def __call__(self):
            return Keystroke(ucs='1', code=1, name='1')

    fake_inkey = iter([FakeInKey1()])
    monkeypatch.setattr('koneko.prompt.TERM.inkey', next(fake_inkey))
    with pytest.raises(CustomExit):
        assert prompt.user_prompt(fakeuser)

    captured = capsys.readouterr()
    assert captured.out == 'Enter a user view command:\n11'


def test_open_or_download_open_coords(monkeypatch):
    mocked_func = Mock()
    mocked_gallery = Mock()
    monkeypatch.setattr('koneko.utils.open_link_coords', mocked_func)
    prompt.open_or_download(mocked_gallery, ['o', '2', '3'])
    assert mocked_func.call_args_list == [call(mocked_gallery._data, 2, 3)]


def test_open_or_download_open_num(monkeypatch):
    mocked_func = Mock()
    mocked_gallery = Mock()
    monkeypatch.setattr('koneko.utils.open_link_num', mocked_func)
    prompt.open_or_download(mocked_gallery, ['O', '2', '3'])
    assert mocked_func.call_args_list == [call(mocked_gallery._data, 23)]


def test_open_or_download_download_coords(monkeypatch):
    mocked_func = Mock()
    mocked_gallery = Mock()
    monkeypatch.setattr('koneko.download.download_image_coords', mocked_func)
    prompt.open_or_download(mocked_gallery, ['d', '2', '3'])
    assert mocked_func.call_args_list == [call(mocked_gallery._data, 2, 3)]


def test_open_or_download_download_num(monkeypatch):
    mocked_func = Mock()
    mocked_gallery = Mock()
    monkeypatch.setattr('koneko.download.download_image_num', mocked_func)
    prompt.open_or_download(mocked_gallery, ['D', '2', '3'])
    assert mocked_func.call_args_list == [call(mocked_gallery._data, 23)]


def test_goto_image_invalid(monkeypatch, capsys):
    mocked_func = Mock()
    mocked_gallery = Mock()
    monkeypatch.setattr('koneko.prompt.gallery_like_prompt', mocked_func)
    prompt.goto_image(mocked_gallery, False)
    assert mocked_func.call_args_list == [call(mocked_gallery)]
    captured = capsys.readouterr()
    assert captured.out == 'Invalid number!\n'


def test_goto_image_valid(monkeypatch):
    mocked_gallery = Mock()
    mocked_gallery.view_image = Mock()
    prompt.goto_image(mocked_gallery, 2)
    assert mocked_gallery.view_image.call_args_list == [call(2)]


def test_goto_image_zero_is_valid(monkeypatch):
    mocked_gallery = Mock()
    mocked_gallery.view_image = Mock()
    prompt.goto_image(mocked_gallery, 0)
    assert mocked_gallery.view_image.call_args_list == [call(0)]

