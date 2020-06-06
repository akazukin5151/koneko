import pytest
from contextlib import contextmanager

from koneko import prompt

@contextmanager
def fakecbreak():
    try:
        yield
    finally:
        pass


def test_ask_quit1(monkeypatch):
    class FakeInKey:
        def __init__(self):
            self.code = True

        def __call__(self):
            return 'n'
    monkeypatch.setattr('koneko.prompt.TERM.cbreak', fakecbreak)
    fake_inkey = FakeInKey  # Doesn't call the class yet
    monkeypatch.setattr('koneko.prompt.TERM.inkey', fake_inkey)
    assert not prompt.ask_quit()

def test_ask_quit2(monkeypatch):
    class FakeInKeyCode:
        def __init__(self):
            self.code = 343

        def __call__(self):
            return 'n'

    monkeypatch.setattr('koneko.prompt.TERM.cbreak', fakecbreak)
    fake_inkey = FakeInKeyCode
    monkeypatch.setattr('koneko.prompt.TERM.inkey', fake_inkey)
    with pytest.raises(SystemExit):
        assert prompt.ask_quit()

    responses = ['y', 'q', '', 'h']
    for letter in responses:
        fake_inkey._call__ = lambda x: letter
        with pytest.raises(SystemExit):
            assert prompt.ask_quit()



