import sys
import pytest
from contextlib import contextmanager

from blessed.keyboard import Keystroke

from koneko import prompt

@contextmanager
def fakecbreak():
    try:
        yield
    finally:
        pass

@pytest.fixture
def patch_cbreak(monkeypatch):
    monkeypatch.setattr('koneko.prompt.TERM.cbreak', fakecbreak)


def test_ask_quit1(monkeypatch, patch_cbreak):
    class FakeInKey:
        def __init__(self):
            self.code = True

    fake_inkey = FakeInKey  # Doesn't call the class yet
    monkeypatch.setattr('koneko.prompt.TERM.inkey', fake_inkey)
    assert not prompt.ask_quit()

def test_ask_quit2(monkeypatch, patch_cbreak):
    class FakeInKeyCode:
        def __init__(self):
            self.code = 343

    fake_inkey = FakeInKeyCode
    monkeypatch.setattr('koneko.prompt.TERM.inkey', fake_inkey)
    with pytest.raises(SystemExit):
        assert prompt.ask_quit()

    responses = ['y', 'q', '', 'h']
    for letter in responses:
        fake_inkey.__call__ = lambda x: letter
        monkeypatch.setattr('koneko.prompt.TERM.inkey', fake_inkey)
        with pytest.raises(SystemExit):
            assert prompt.ask_quit()


class FakeInKey:
    def __init__(self):
        self.code = True

    def isdigit(self):
        return False

    def __call__(self):
        return Keystroke(ucs=u'n', code=1, name=u'next')

class FakeGallery:
    def __init__(self):
        self.data = None

    def next_page(self):
        sys.exit(0)

    def handle_prompt(self, *a):
        sys.exit(0)

    @staticmethod
    def help():
        sys.exit(0)

def test_gallery_like_prompt(monkeypatch, patch_cbreak):
    for letter in (u'n', u'h', u'b', u'r'):
        class FakeInKeyNew(FakeInKey):
            def __call__(self):
                return Keystroke(ucs=letter, code=1, name=letter)

        fake_inkey = FakeInKeyNew()
        monkeypatch.setattr('koneko.prompt.TERM.inkey', fake_inkey)
        fakegallery = FakeGallery()
        with pytest.raises(SystemExit):
            assert prompt.gallery_like_prompt(fakegallery)

def test_gallery_like_prompt_previous(monkeypatch, patch_cbreak):
    class FakeInKeyPrev(FakeInKey):
        def __call__(self):
            return Keystroke(ucs=u'p', code=1, name=u'p')

    fake_inkey = FakeInKeyPrev()
    monkeypatch.setattr('koneko.prompt.TERM.inkey', fake_inkey)
    monkeypatch.setattr('koneko.ui.previous_page', lambda x: sys.exit(0))
    fakegallery = FakeGallery()
    with pytest.raises(SystemExit):
        assert prompt.gallery_like_prompt(fakegallery)

def test_gallery_like_prompt_ask_quit(monkeypatch, patch_cbreak):
    class FakeInKeyQuit(FakeInKey):
        def __call__(self):
            return Keystroke(ucs=u'q', code=1, name=u'quit')

    fake_inkey = FakeInKeyQuit()
    monkeypatch.setattr('koneko.prompt.TERM.inkey', fake_inkey)
    monkeypatch.setattr('koneko.prompt.ask_quit', lambda: sys.exit(0))
    fakegallery = FakeGallery()
    with pytest.raises(SystemExit):
        assert prompt.gallery_like_prompt(fakegallery)
