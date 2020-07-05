import sys
import pytest

from koneko import printer

# Lmao python
sys.path.append('testing')

from page_json import page_json  # isort:skip

page_illusts = page_json["illusts"]

def test_print_multiple_imgs(capsys):
    assert printer.print_multiple_imgs(page_illusts) is None
    captured = capsys.readouterr()
    assert captured.out == "\x1b[31m#14\x1b[39m has \x1b[34m8\x1b[39m pages, \x1b[31m#25\x1b[39m has \x1b[34m50\x1b[39m pages, \n"

