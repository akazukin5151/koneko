import json

import pytest

from koneko import printer

from testing.page_json import page_json  # isort:skip

page_illusts = page_json['illusts']

with open('testing/files/mode1.json', 'r') as json_file:
    page_illusts_new = json.load(json_file)['illusts']


def test_print_multiple_imgs_two_posts(capsys):
    printer.print_multiple_imgs(page_illusts)
    captured = capsys.readouterr()
    assert captured.out == '\x1b[31m#14\x1b[39m has \x1b[34m8\x1b[39m pages, \x1b[31m#25\x1b[39m has \x1b[34m50\x1b[39m pages, \n'

def test_print_multiple_imgs_one_post(capsys):
    printer.print_multiple_imgs(page_illusts_new)
    captured = capsys.readouterr()
    assert captured.out == '\x1b[31m#22\x1b[39m has \x1b[34m8\x1b[39m pages, \n'

def test_write(capsys):
    printer.write('hi')
    captured = capsys.readouterr()
    assert captured.out == 'hi'


def test_cursor_move_up(capsys):
    printer.move_cursor_up(1)
    captured = capsys.readouterr()
    assert captured.out == '\033[1A'

    printer.move_cursor_up(42)
    captured = capsys.readouterr()
    assert captured.out == '\033[42A'


def test_cursor_move_down(capsys):
    printer.move_cursor_down(1)
    captured = capsys.readouterr()
    assert captured.out == '\033[1B'

    printer.move_cursor_down(42)
    captured = capsys.readouterr()
    assert captured.out == '\033[42B'


def test_cursor_move_xy(capsys):
    printer.move_cursor_xy(1, 2)
    captured = capsys.readouterr()
    assert captured.out == '\033[1;2H'

    printer.move_cursor_xy(42, 42)
    captured = capsys.readouterr()
    assert captured.out == '\033[42;42H'


def test_erase_line(capsys):
    printer.erase_line()
    captured = capsys.readouterr()
    assert captured.out == '\033[K'


def test_print_info(capsys):
    printer._print_info(10)
    captured = capsys.readouterr()
    assert captured.out == '          000\n          Example artist\n'

    printer._print_info(5)
    captured = capsys.readouterr()
    assert captured.out == '     000\n     Example artist\n'


@pytest.mark.parametrize('correct', ([1], [8], [1, 8]))
@pytest.mark.parametrize('incorrect', ([2], [2, 3]))
def test_maybe_print_size(capsys, correct, incorrect):
    printer.maybe_print_size(correct, 10)
    captured = capsys.readouterr()
    assert captured.out == 'thumbnail_size = 10\n'

    printer.maybe_print_size(incorrect, 10)
    captured = capsys.readouterr()
    assert captured.out == ''


@pytest.mark.parametrize('correct', ([2], [8], [2, 8]))
@pytest.mark.parametrize('incorrect', ([1], [1, 3]))
def test_maybe_print_width_xpadding(capsys, correct, incorrect):
    printer.maybe_print_width_xpadding(correct, 10, 20)
    captured = capsys.readouterr()
    assert captured.out == 'image_width = 10\nimages_x_spacing = 20\n'

    printer.maybe_print_width_xpadding(incorrect, 10, 20)
    captured = capsys.readouterr()
    assert captured.out == ''


@pytest.mark.parametrize('correct', ([3], [8], [3, 8]))
@pytest.mark.parametrize('incorrect', ([1], [1, 2]))
def test_maybe_print_height_ypadding(capsys, correct, incorrect):
    printer.maybe_print_height_ypadding(correct, 10, 20)
    captured = capsys.readouterr()
    assert captured.out == 'image_height = 10\nimages_y_spacing = 20\n'

    printer.maybe_print_height_ypadding(incorrect, 10, 20)
    captured = capsys.readouterr()
    assert captured.out == ''


@pytest.mark.parametrize('correct', ([4], [8], [4, 8]))
@pytest.mark.parametrize('incorrect', ([1], [1, 2]))
def test_maybe_print_page_spacing(capsys, correct, incorrect):
    printer.maybe_print_page_spacing(correct, 10)
    captured = capsys.readouterr()
    assert captured.out == 'page_spacing = 10\n'

    printer.maybe_print_page_spacing(incorrect, 10)
    captured = capsys.readouterr()
    assert captured.out == ''



@pytest.mark.parametrize('correct', ([5], [8], [5, 8]))
@pytest.mark.parametrize('incorrect', ([1], [1, 2]))
def test_maybe_print_print_spacing(capsys, correct, incorrect):
    printer.maybe_print_print_spacing(correct, range(5))
    captured = capsys.readouterr()
    assert captured.out == 'gallery_print_spacing = 0,1,2,3,4\n'

    printer.maybe_print_print_spacing(incorrect, 10)
    captured = capsys.readouterr()
    assert captured.out == ''


@pytest.mark.parametrize('correct', ([6], [8], [6, 8]))
@pytest.mark.parametrize('incorrect', ([1], [1, 2]))
def test_maybe_print_user_info(capsys, correct, incorrect):
    printer.maybe_print_user_info(correct, 10)
    captured = capsys.readouterr()
    assert captured.out == 'users_print_name_xcoord = 10\n'

    printer.maybe_print_user_info(incorrect, 10)
    captured = capsys.readouterr()
    assert captured.out == ''


def test_print_doc(capsys, monkeypatch):
    monkeypatch.setattr('koneko.Terminal.height', 50)
    printer.print_doc('hi')
    captured = capsys.readouterr()
    assert captured.out == 'hi\n'

    printer.print_doc('hi\n\n')
    captured = capsys.readouterr()
    assert captured.out == 'hi\n\n\n'


def test_update_gallery_info(capsys):
    printer.update_gallery_info(range(5), 5, 1)
    captured = capsys.readouterr()
    assert captured.out == '\x1b[2A\x1b[K1 2  3   4    5\n\nAdjusting the number of spaces between 1 and 2\n\x1b[1A'


def test_update_user_info(capsys):
    printer.update_user_info(5)
    captured = capsys.readouterr()
    assert captured.out == '\x1b[K\x1b[1B\x1b[K\x1b[1A     000\n     Example artist\n\x1b[2A'


def test_image_help(capsys):
    printer.image_help()
    captured = capsys.readouterr()
    assert captured.out == '\n\x1b[31m[\x1b[35mb\x1b[31m]\x1b[39mack; \x1b[31m[\x1b[35mn\x1b[31m]\x1b[39mext image; \x1b[31m[\x1b[35mp\x1b[31m]\x1b[39mrevious image; \x1b[31m[\x1b[35md\x1b[31m]\x1b[39mownload image;\x1b[31m[\x1b[35mo\x1b[31m]\x1b[39mpen image in browser;\nshow image in\x1b[31m[\x1b[35mf\x1b[31m]\x1b[39mull res; \x1b[31m[\x1b[35mq\x1b[31m]\x1b[39muit (with confirmation); view \x1b[31m[\x1b[35mm\x1b[31m]\x1b[39manual\n\n'



def test_user_help(capsys):
    printer.user_help()
    captured = capsys.readouterr()
    assert captured.out == "\nview \x1b[31m[\x1b[34mn\x1b[31m]\x1b[39mth artist's illusts \x1b[31m[\x1b[35mn\x1b[31m]\x1b[39mext page; \x1b[31m[\x1b[35mp\x1b[31m]\x1b[39mrevious page; \x1b[31m[\x1b[35mr\x1b[31m]\x1b[39meload and re-download all;\n\x1b[31m[\x1b[35mq\x1b[31m]\x1b[39muit (with confirmation);view \x1b[31m[\x1b[35mm\x1b[31m]\x1b[39manual\n\n"

