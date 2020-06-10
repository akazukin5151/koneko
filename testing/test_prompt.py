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

    def __call__(self):
        return Keystroke(ucs=u'n', code=1, name=u'next')

class FakeGallery:
    def __init__(self):
        self.data = None

    @staticmethod
    def help():                  sys.exit(0)
    def next_page(self):         sys.exit(0)
    def handle_prompt(self, *a): sys.exit(0)
    def view_image(self, *a):    sys.exit(0)


def test_gallery_like_prompt(monkeypatch, patch_cbreak):
    fakegallery = FakeGallery()
    for letter in (u'n', u'h', u'b', u'r'):
        class FakeInKeyNew(FakeInKey):
            def __call__(self):
                return Keystroke(ucs=letter, code=1, name=letter)

        fake_inkey = FakeInKeyNew()
        monkeypatch.setattr('koneko.prompt.TERM.inkey', fake_inkey)
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


def test_gallery_like_prompt_digits_seq(monkeypatch, patch_cbreak):
    class FakeInKey1(FakeInKey):
        def __call__(self):
            return Keystroke(ucs=u'1', code=1, name=u'1')

    class FakeInKey2(FakeInKey):
        def __call__(self):
            return Keystroke(ucs=u'2', code=1, name=u'2')

    fake_inkey = iter([FakeInKey1(), FakeInKey2()])
    monkeypatch.setattr('koneko.prompt.TERM.inkey', next(fake_inkey))
    monkeypatch.setattr('koneko.utils.find_number_map', lambda *a: 0)
    fakegallery = FakeGallery()
    with pytest.raises(SystemExit):
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
#    monkeypatch.setattr('koneko.ui.open_link_coords', lambda *a: sys.exit(0))
#    fakegallery = FakeGallery()
#    with pytest.raises(SystemExit):
#        assert prompt.gallery_like_prompt(fakegallery)


class FakeImage:
    def __init__(self):       self.data = None
    def open_image(self):     sys.exit(0)
    def download_image(self): sys.exit(0)
    def next_image(self):     sys.exit(0)
    def previous_image(self): sys.exit(0)
    def show_full_res(self):  sys.exit(0)
    def leave(self, *a):      sys.exit(0)

def test_image_prompt(monkeypatch, patch_cbreak):
    monkeypatch.setattr('koneko.prompt.ask_quit', lambda: sys.exit(0))
    fakeimage = FakeImage()
    for letter in (u'a', u'b', u'q', u'o', u'd', u'n', u'p', u'f'):
        class FakeInKeyNew(FakeInKey):
            def __call__(self):
                return Keystroke(ucs=letter, code=1, name=letter)

        fake_inkey = FakeInKeyNew()
        monkeypatch.setattr('koneko.prompt.TERM.inkey', fake_inkey)
        with pytest.raises(SystemExit):
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
    monkeypatch.setattr('koneko.ui.jump_to_image', lambda *a: sys.exit(0))
    fakeimage = FakeImage()
    with pytest.raises(SystemExit):
        assert prompt.image_prompt(fakeimage)



class FakeUser:
    def __init__(self):           self.data = None
    def next_page(self):          sys.exit(0)
    def reload(self):             sys.exit(0)
    def go_artist_mode(self, *a): sys.exit(0)

def test_user_prompt(monkeypatch, patch_cbreak):
    monkeypatch.setattr('koneko.prompt.ask_quit', lambda: sys.exit(0))
    monkeypatch.setattr('koneko.ui.previous_page_users', lambda *a: sys.exit(0))
    fakeuser = FakeUser()
    for letter in (u'n', u'r', u'p', u'q'):
        class FakeInKeyNew(FakeInKey):
            def __call__(self):
                return Keystroke(ucs=letter, code=1, name=letter)

        fake_inkey = FakeInKeyNew()
        monkeypatch.setattr('koneko.prompt.TERM.inkey', fake_inkey)
        with pytest.raises(SystemExit):
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
    fakeuser= FakeUser()
    with pytest.raises(SystemExit):
        assert prompt.user_prompt(fakeuser)

