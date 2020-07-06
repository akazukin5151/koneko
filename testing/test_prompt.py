import sys
from abc import ABC, abstractmethod
from contextlib import contextmanager

import pytest
from blessed.keyboard import Keystroke

from koneko import prompt
from conftest import CustomExit, raises_customexit


@contextmanager
def fakecbreak():
    try:
        yield
    finally:
        pass

@pytest.fixture
def patch_cbreak(monkeypatch):
    monkeypatch.setattr('koneko.prompt.TERM.cbreak', fakecbreak)

@pytest.fixture
def customexit_to_quit(monkeypatch):
    monkeypatch.setattr('koneko.prompt.sys.exit', raises_customexit)


def test_ask_quit_do_nothing(monkeypatch, patch_cbreak, customexit_to_quit):
    class FakeInKey:
        def __init__(self):
            self.code = True

    fake_inkey = FakeInKey  # Doesn't call the class yet
    monkeypatch.setattr('koneko.prompt.TERM.inkey', fake_inkey)
    assert not prompt.ask_quit()

def test_ask_quit_enter_and_letter_quits(monkeypatch, patch_cbreak, customexit_to_quit):
    class FakeInKeyCode:
        def __init__(self):
            self.code = 343

    fake_inkey = FakeInKeyCode
    monkeypatch.setattr('koneko.prompt.TERM.inkey', fake_inkey)
    with pytest.raises(CustomExit):
        assert prompt.ask_quit()

    responses = ['y', 'q', '', 'h']
    for letter in responses:
        fake_inkey.__call__ = lambda x: letter
        monkeypatch.setattr('koneko.prompt.TERM.inkey', fake_inkey)
        with pytest.raises(CustomExit):
            assert prompt.ask_quit()


class FakeInKey(ABC):
    """Fakes the Keystroke constructor in blessed. See:
    https://github.com/jquast/blessed/blob/83748bd9e97f9f2fd849588e677d541e162e3fc6/tests/test_keyboard.py#L98
    """
    def __init__(self):
        self.code = True

    @abstractmethod
    def __call__(self):
        raise NotImplementedError

class FakeGallery:
    @staticmethod
    def help():                  raise CustomExit()
    def next_page(self):         raise CustomExit()
    def previous_page(self):     raise CustomExit()
    def handle_prompt(self, *a): raise CustomExit()
    def view_image(self, *a):    raise CustomExit()
    def __init__(self):          self.data = None


def test_gallery_like_prompt(monkeypatch, patch_cbreak):
    fakegallery = FakeGallery()
    for letter in (u'n', u'h', u'b', u'r'):
        class FakeInKeyNew(FakeInKey):
            def __call__(self):
                return Keystroke(ucs=letter, code=1, name=letter)

        fake_inkey = FakeInKeyNew()
        monkeypatch.setattr('koneko.prompt.TERM.inkey', fake_inkey)
        with pytest.raises(CustomExit):
            assert prompt.gallery_like_prompt(fakegallery)

def test_gallery_like_prompt_previous(monkeypatch, patch_cbreak):
    class FakeInKeyPrev(FakeInKey):
        def __call__(self):
            return Keystroke(ucs=u'p', code=1, name=u'p')

    fake_inkey = FakeInKeyPrev()
    monkeypatch.setattr('koneko.prompt.TERM.inkey', fake_inkey)
    monkeypatch.setattr('koneko.ui.AbstractGallery.previous_page', raises_customexit)
    fakegallery = FakeGallery()
    with pytest.raises(CustomExit):
        assert prompt.gallery_like_prompt(fakegallery)

def test_gallery_like_prompt_ask_quit(monkeypatch, patch_cbreak):
    class FakeInKeyQuit(FakeInKey):
        def __call__(self):
            return Keystroke(ucs=u'q', code=1, name=u'quit')

    fake_inkey = FakeInKeyQuit()
    monkeypatch.setattr('koneko.prompt.TERM.inkey', fake_inkey)
    monkeypatch.setattr('koneko.prompt.ask_quit', raises_customexit)
    fakegallery = FakeGallery()
    with pytest.raises(CustomExit):
        assert prompt.gallery_like_prompt(fakegallery)


def test_gallery_like_prompt_digits_seq(monkeypatch, patch_cbreak):
    class FakeInKey1(FakeInKey):
        def __call__(self):
            return Keystroke(ucs=u'1', code=1, name=u'1')

    class FakeInKey2(FakeInKey):
        def __call__(self):
            return Keystroke(ucs=u'2', code=1, name=u'2')

    fake_inkey = iter([FakeInKey1(), FakeInKey2()])
    monkeypatch.setattr('koneko.prompt.TERM.inkey', next(fake_inkey))
    fakegallery = FakeGallery()
    with pytest.raises(CustomExit):
        assert prompt.gallery_like_prompt(fakegallery)

# Doesn't work for some reason, but sequencable_keys did trigger
#def test_gallery_like_prompt_3_seq(monkeypatch, patch_cbreak):
#    class FakeInKey0(FakeInKey):
#        def __call__(self):
#            return Keystroke(ucs=u'o', code=1, name=u'o')
#
#    class FakeInKey1(FakeInKey):
#        def __call__(self):
#            return Keystroke(ucs=u'1', code=1, name=u'1')
#
#    class FakeInKey2(FakeInKey):
#        def __call__(self):
#            return Keystroke(ucs=u'2', code=1, name=u'2')
#
#    fake_inkey = iter([FakeInKey0(), FakeInKey1(), FakeInKey2()])
#    monkeypatch.setattr('koneko.prompt.TERM.inkey', next(fake_inkey))
#    monkeypatch.setattr('koneko.utils.open_link_coords', lambda *a: sys.exit(0))
#    fakegallery = FakeGallery()
#    with pytest.raises(SystemExit):
#        assert prompt.gallery_like_prompt(fakegallery)


class FakeImage:
    def __init__(self):       self.data = None
    def open_image(self):     raise CustomExit()
    def download_image(self): raise CustomExit()
    def next_image(self):     raise CustomExit()
    def previous_image(self): raise CustomExit()
    def show_full_res(self):  raise CustomExit()
    def leave(self, *a):      raise CustomExit()

def test_image_prompt(monkeypatch, patch_cbreak):
    monkeypatch.setattr('koneko.prompt.ask_quit', raises_customexit)
    fakeimage = FakeImage()
    for letter in (u'a', u'b', u'q', u'o', u'd', u'n', u'p', u'f'):
        class FakeInKeyNew(FakeInKey):
            def __call__(self):
                return Keystroke(ucs=letter, code=1, name=letter)

        fake_inkey = FakeInKeyNew()
        monkeypatch.setattr('koneko.prompt.TERM.inkey', fake_inkey)
        with pytest.raises(CustomExit):
            assert prompt.image_prompt(fakeimage)

def test_image_prompt_seq(monkeypatch, patch_cbreak):
    class FakeInKey1(FakeInKey):
        def __call__(self):
            return Keystroke(ucs=u'1', code=1, name=u'1')

    class FakeInKey2(FakeInKey):
        def __call__(self):
            return Keystroke(ucs=u'2', code=1, name=u'2')

    fake_inkey = iter([FakeInKey1(), FakeInKey2()])
    monkeypatch.setattr('koneko.prompt.TERM.inkey', next(fake_inkey))
    monkeypatch.setattr('koneko.ui.jump_to_image', raises_customexit)
    fakeimage = FakeImage()
    with pytest.raises(CustomExit):
        assert prompt.image_prompt(fakeimage)



class FakeUser:
    def __init__(self):           self.data = None
    def next_page(self):          raise CustomExit()
    def previous_page(self):      raise CustomExit()
    def reload(self):             raise CustomExit()
    def go_artist_mode(self, *a): raise CustomExit()

def test_user_prompt(monkeypatch, patch_cbreak):
    monkeypatch.setattr('koneko.prompt.ask_quit', raises_customexit)
    monkeypatch.setattr('koneko.ui.AbstractUsers.previous_page', raises_customexit)
    fakeuser = FakeUser()
    for letter in (u'n', u'r', u'p', u'q'):
        class FakeInKeyNew(FakeInKey):
            def __call__(self):
                return Keystroke(ucs=letter, code=1, name=letter)

        fake_inkey = FakeInKeyNew()
        monkeypatch.setattr('koneko.prompt.TERM.inkey', fake_inkey)
        with pytest.raises(CustomExit):
            assert prompt.user_prompt(fakeuser)

def test_user_prompt_seq(monkeypatch, patch_cbreak):
    class FakeInKey1(FakeInKey):
        def __call__(self):
            return Keystroke(ucs=u'1', code=1, name=u'1')

    class FakeInKey2(FakeInKey):
        def __call__(self):
            return Keystroke(ucs=u'2', code=1, name=u'2')

    fake_inkey = iter([FakeInKey1(), FakeInKey2()])
    monkeypatch.setattr('koneko.prompt.TERM.inkey', next(fake_inkey))
    fakeuser = FakeUser()
    with pytest.raises(CustomExit):
        assert prompt.user_prompt(fakeuser)
