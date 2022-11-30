"""Doesn't do anything too advanced (can't figure out multi-key sequences yet), so just returns defaults"""
from sys import platform
from unittest.mock import Mock

import pytest

from koneko import assistants, config
from conftest import setup_test_config


@pytest.fixture
def disable_pixcat(monkeypatch):
    # pixcat.Image now won't bother us with AttributeErrors and do nothing
    # Specific to this module, so can't extract to conftest
    monkeypatch.setattr('koneko.lscat.api', Mock())


@pytest.fixture
def disable_print_doc(monkeypatch):
    monkeypatch.setattr('koneko.printer.print_doc', lambda x: True)


class FakeInKey:
    def __init__(self):
        self.name = 'KEY_ENTER'


def test_thumbnail_size_assistant_default(monkeypatch, disable_pixcat, patch_cbreak, disable_print_doc):
    monkeypatch.setattr('koneko.TERM.inkey', FakeInKey)
    monkeypatch.setattr('koneko.config.api.use_ueberzug', lambda: False)
    assert assistants.thumbnail_size_assistant() == 300  # Default


def test_page_spacing_assistant(monkeypatch, disable_pixcat, capsys):
    monkeypatch.setattr('koneko.config.api.use_ueberzug', lambda: False)
    responses = iter(['', 'not_a_number', '30'])
    monkeypatch.setattr('builtins.input', lambda *a: next(responses))
    monkeypatch.setattr('koneko.assistants.time.sleep', lambda *a, **k: Mock())
    monkeypatch.setattr('koneko.assistants.os.system', lambda *a, **k: Mock())

    monkeypatch.setattr('koneko.Terminal.height', 40)

    assert assistants.page_spacing_assistant(310) == '30'
    captured = capsys.readouterr()
    assert captured.out == '=== Page spacing ===\nThis will display an image, then print newlines.\nYour desired setting is the number when the image completely scrolls out of view\n0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15\n16\n17\n18\n19\n20\n21\n22\n23\n24\n25\n26\n27\n28\n29\n30\n31\n32\n33\n34\n35\n36\n37\n38\n39\n40\n41\n42\n43\n44\nWhen the image just completely scrolls out of view, what is the largest number?\n(By default on kitty, ctrl+shift+up/down scrolls up/down a line)\nMust enter a number!\n'


def test_page_spacing_assistant_must_not_use_ueberzug(monkeypatch, capsys):
    monkeypatch.setattr('koneko.config.api.use_ueberzug', lambda: True)
    assert assistants.page_spacing_assistant(310) == None
    captured = capsys.readouterr()
    assert captured.out == 'The page spacing assistant is not needed if you use ueberzug, because ueberzug does not respond to scroll events\n'


def test_gallery_print_spacing_assistant_n(monkeypatch, disable_pixcat, disable_print_doc, patch_cbreak, capsys, tmp_path):
    setup_test_config(tmp_path, config.Config)
    monkeypatch.setattr('koneko.config.api.use_ueberzug', lambda: False)
    monkeypatch.setattr('koneko.Terminal.width', 40)
    monkeypatch.setattr('builtins.input', lambda: '')

    monkeypatch.setattr('koneko.TERM.inkey', FakeInKey)
    # Initially all spaces are 1s
    assert assistants.gallery_print_spacing_assistant(310, 1, 10, 10) == [1, 1, 1]
    captured = capsys.readouterr()
    assert captured.out == '\n\n\x1b[2A\x1b[K 1 2 3\n\nAdjusting the number of spaces between 0 and 1\n\x1b[1A'

def test_gallery_print_spacing_assistant_n_ueberzug(monkeypatch, disable_pixcat, disable_print_doc, patch_cbreak, capsys, tmp_path):
    setup_test_config(tmp_path, config.Config)
    monkeypatch.setattr('koneko.config.api.use_ueberzug', lambda: True)
    monkeypatch.setattr('koneko.Terminal.width', 40)
    monkeypatch.setattr('builtins.input', lambda: '')

    monkeypatch.setattr('koneko.TERM.inkey', FakeInKey)
    # Initially all spaces are 1s
    assert assistants.gallery_print_spacing_assistant(310, 1, 10, 10) == [1, 1, 1]
    captured = capsys.readouterr()
    assert captured.out == '\n\n\n\n\n\n\n\n\n\n\n\x1b[2A\x1b[K 1 2 3\n\nAdjusting the number of spaces between 0 and 1\n\x1b[1A'



# TODO: test config is needed but this relies on the files being properly named
#def test_gallery_print_spacing_assistant_y(monkeypatch, disable_print_doc, patch_cbreak, capsys, tmp_path):
#    setup_test_config(tmp_path, config.Config)
#    monkeypatch.setattr('koneko.config.api.use_ueberzug', lambda: False)
#    monkeypatch.setattr('koneko.Terminal.width', 40)
#    monkeypatch.setattr('builtins.input', lambda: 'y')
#    monkeypatch.setattr('koneko.picker.pick_dir', lambda *a: tmp_path)
#    monkeypatch.setattr('koneko.lscat_app.FakeData', lambda *a: True)
#    monkeypatch.setattr('koneko.lscat.show_instant', lambda *a: True)
#
#    monkeypatch.setattr('koneko.TERM.inkey', FakeInKey)
#    # Initially all spaces are 1s
#    assert assistants.gallery_print_spacing_assistant(310, 1, 10, 10) == [1, 1]
#    captured = capsys.readouterr()
#    assert captured.out == '\n\n\x1b[2A\x1b[K 1 2\n\nAdjusting the number of spaces between 0 and 1\n\x1b[1A'


# TODO: test config is needed but this relies on the files being properly named
#@pytest.mark.skipif(platform == 'darwin', reason='Ueberzug does not work on macOS')
#def test_gallery_print_spacing_assistant_y_ueberzug(monkeypatch, disable_print_doc, patch_cbreak, capsys, tmp_path):
#    monkeypatch.setattr('koneko.config.api.use_ueberzug', lambda: True)
#    monkeypatch.setattr('koneko.Terminal.width', 40)
#    monkeypatch.setattr('koneko.Terminal.height', 25)  # Fixes pytest if launched with -s
#    monkeypatch.setattr('builtins.input', lambda: 'y')
#    monkeypatch.setattr('koneko.picker.pick_dir', lambda *a: tmp_path)
#    monkeypatch.setattr('koneko.lscat_app.FakeData', lambda *a: True)
#    monkeypatch.setattr('koneko.lscat.show_instant', lambda *a: True)
#
#    monkeypatch.setattr('koneko.TERM.inkey', FakeInKey)
#    # Default
#    assert assistants.gallery_print_spacing_assistant(310, 1, 10, 10) == [1, 1]
#    captured = capsys.readouterr()
#    assert captured.out == '\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\x1b[2A\x1b[K 1 2\n\nAdjusting the number of spaces between 0 and 1\n\x1b[1A'


def test_user_info_assistant(monkeypatch, disable_print_doc, disable_pixcat, patch_cbreak, capsys):
    monkeypatch.setattr('koneko.config.api.use_ueberzug', lambda: False)
    monkeypatch.setattr('koneko.Terminal.width', 40)
    monkeypatch.setattr('koneko.lscat.api.show_user_row', lambda *a: [Mock()])

    monkeypatch.setattr('koneko.TERM.inkey', FakeInKey)
    # Default
    assert assistants.user_info_assistant(310, 10, 1) == 18
    captured = capsys.readouterr()
    assert captured.out == '\x1b[5A\x1b[K\x1b[1B\x1b[K\x1b[1A                  000\n                  Example artist\n\x1b[2A\n\n\n\n'


def test_xpadding_assistant(monkeypatch, disable_pixcat, patch_cbreak, disable_print_doc, capsys):
    monkeypatch.setattr('koneko.TERM.inkey', FakeInKey)
    monkeypatch.setattr('koneko.lscat.show_single_x', lambda *a, **k: Mock())
    # Default
    assert assistants.xpadding_assistant(310) == (0, 0)

    captured = capsys.readouterr()
    assert captured.out == '\r                    \rimage width = 0\x1b[K\r                    \rx spacing = 0    '


def test_ypadding_assistant(monkeypatch, disable_pixcat, patch_cbreak, disable_print_doc, capsys):
    monkeypatch.setattr('koneko.TERM.inkey', FakeInKey)
    monkeypatch.setattr('koneko.lscat.show_single_x', lambda *a, **k: Mock())
    monkeypatch.setattr('koneko.lscat.show_single_y', lambda *a, **k: Mock())
    # Default
    assert assistants.ypadding_assistant(310) == (0, 0)

    captured = capsys.readouterr()
    assert captured.out == '\r                    \rimage height = 0\x1b[K\r                    \ry spacing = 0      '


def test_center_spaces_assistant(monkeypatch, disable_pixcat, patch_cbreak, disable_print_doc, capsys):
    monkeypatch.setattr('koneko.config.api.use_ueberzug', lambda: True)
    monkeypatch.setattr('koneko.Terminal.width', 40)
    monkeypatch.setattr('koneko.Terminal.height', 25)  # Fixes pytest if launched with -s
    monkeypatch.setattr('koneko.lscat.api', Mock())

    monkeypatch.setattr('koneko.TERM.inkey', FakeInKey)
    # Default
    assert assistants.center_spaces_assistant() == 0
    captured = capsys.readouterr()
    assert captured.out == '\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n Current position: 0\n\x1b[1ACurrent position: 00\n'


def test_center_spaces_assistant_must_use_ueberzug(monkeypatch, capsys):
    monkeypatch.setattr('koneko.config.api.use_ueberzug', lambda: False)
    assert assistants.center_spaces_assistant() == -1
    captured = capsys.readouterr()
    assert captured.out == 'Center images assistant is only for ueberzug!\n'
