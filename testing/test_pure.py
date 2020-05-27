import os
import sys
from pathlib import Path

import pytest

# Lmao python
sys.path.append('../koneko/koneko')
sys.path.append('testing')

from koneko.koneko import pure, lscat, utils, data, KONEKODIR
from page_json import *  # Imports the current_page (dict) stored in disk

page_illusts = page_json["illusts"]


# From pure.py
def test_cd():
    current_dir = os.getcwd()
    with pure.cd(current_dir):
        testdir = os.getcwd()

    assert testdir == os.getcwd()
    assert os.getcwd() == current_dir


def test_split_backslash_last():
    assert (
        pure.split_backslash_last("https://www.pixiv.net/en/users/2232374") == "2232374"
    )
    assert (
        pure.split_backslash_last("https://www.pixiv.net/en/artworks/78823485")
        == "78823485"
    )


def test_generate_filepath():
    assert (
        pure.generate_filepath("78823485_p0.jpg")
        == Path('~/Downloads/78823485_p0.jpg').expanduser()
    )


def test_prefix_filename():
    assert pure.prefix_filename("old.jpg", "new", 2) == "002_new.jpg"
    assert pure.prefix_filename("old.jpg", "new", 10) == "010_new.jpg"

def test_prefix_artist_name():
    assert pure.prefix_artist_name("name1", 2) == f"02\n{' ' * 19}name1"
    assert pure.prefix_artist_name("name2", 10) == f"10\n{' ' * 19}name2"

def test_find_number_map():
    assert ([pure.find_number_map(x, y)
             for y in range(1,7)
             for x in range(1,6)] == list(range(30)))
    assert pure.find_number_map(0, 100) == False


def test_print_multiple_imgs(capsys):
    assert pure.print_multiple_imgs(page_illusts) == None
    captured = capsys.readouterr()
    assert captured.out == "\x1b[31m#14\x1b[39m has \x1b[34m8\x1b[39m pages, \x1b[31m#25\x1b[39m has \x1b[34m50\x1b[39m pages, \n"


def test_url_given_size():
    assert (
        pure.url_given_size(page_illusts[0], "medium")
        == "https://i.pximg.net/c/540x540_70/img-master/img/2020/03/10/04/07/08/80017594_p0_master1200.jpg"
    )
    assert (
        pure.url_given_size(page_illusts[1], "large")
        == "https://i.pximg.net/c/600x1200_90_webp/img-master/img/2020/02/29/19/09/35/79799236_p0_master1200.jpg"
    )


def test_post_title():
    assert pure.post_title(page_illusts, 0) == "310"
    assert pure.post_title(page_illusts, 1) == "Midnight Sun"


def test_medium_urls():
    assert len(pure.medium_urls(page_illusts)) == 30
    assert (
        pure.medium_urls(page_illusts)[0]
        == "https://i.pximg.net/c/540x540_10_webp/img-master/img/2020/03/10/04/07/08/80017594_p0_square1200.jpg"
    )


def test_page_urls_in_post():
    assert len(pure.page_urls_in_post(page_illusts[14], size="medium")) == 2
    assert pure.page_urls_in_post(page_illusts[14], size="medium")[0] == 8
    assert len(pure.page_urls_in_post(page_illusts[14], size="medium")[1]) == 8
    assert pure.page_urls_in_post(page_illusts[0], size="medium")[0] == 1
    assert len(pure.page_urls_in_post(page_illusts[0], size="medium")[1]) == 1


def test_post_titles_in_page():
    assert len(pure.post_titles_in_page(page_illusts)) == 30
    assert pure.post_titles_in_page(page_illusts)[0] == "310"
    assert pure.post_titles_in_page(page_illusts)[1] == "Midnight Sun"


def test_change_url_to_full():
    assert (
        pure.change_url_to_full(page_illusts[0], png=False)
        == "https://i.pximg.net/img-original/img/2020/03/10/04/07/08/80017594_p0.jpg"
    )
    # Isn't actually needed for this image, but just testing
    assert (
        pure.change_url_to_full(page_illusts[0], png=True)
        == "https://i.pximg.net/img-original/img/2020/03/10/04/07/08/80017594_p0.png"
    )

def test_process_user_url():
    assert (pure.process_user_url("https://www.pixiv.net/en/users/2232374") ==
            ("2232374", "1"))
    assert pure.process_user_url("2232374") == ("2232374", "1")

def test_process_artwork_url():
    assert (pure.process_artwork_url("https://www.pixiv.net/en/artworks/76695217") ==
            ("76695217", "2"))
    assert pure.process_artwork_url("http://www.pixiv.net/member_illust.php?mode=medium&illust_id=76695217") == ("76695217", "2")
    assert pure.process_artwork_url("76695217") == ("76695217", "2")
