import sys
import json
from pathlib import Path

from koneko import pure

# Imports the current_page (dict) stored in disk
from testing.page_json import page_json  # isort:skip

page_illusts = page_json['illusts']


with open('testing/files/mode1.json', 'r') as json_file:
    page_illusts_new = json.load(json_file)['illusts']

def test_split_backslash_last():
    assert (
        pure.split_backslash_last('https://www.pixiv.net/en/users/2232374') == '2232374'
    )
    assert (
        pure.split_backslash_last('https://www.pixiv.net/en/artworks/78823485')
        == '78823485'
    )


def test_generate_filepath():
    assert (
        pure._generate_filepath('78823485_p0.jpg')
        == Path('~/Downloads/78823485_p0.jpg').expanduser()
    )


def test_prefix_filename():
    assert pure._prefix_filename('old.jpg', 'new', 2) == '002_new.jpg'
    assert pure._prefix_filename('old.jpg', 'new', 10) == '010_new.jpg'


def test_url_given_size():
    assert (
        pure.url_given_size(page_illusts_new[0], 'medium')
        == 'https://i.pximg.net/c/540x540_70/img-master/img/2020/05/14/06/45/24/81547984_p0_master1200.jpg'
    )
    assert (
        pure.url_given_size(page_illusts_new[1], 'large')
        == 'https://i.pximg.net/c/600x1200_90_webp/img-master/img/2020/05/12/06/36/27/81501385_p0_master1200.jpg'
    )


def test_post_title():
    assert pure.post_title(page_illusts_new, 0) == 'みこっちゃん'
    assert pure.post_title(page_illusts_new, 1) == 'おりじなる'


def test_medium_urls():
    assert len(pure.medium_urls(page_illusts_new)) == 30
    assert (
        pure.medium_urls(page_illusts_new)[0]
        == 'https://i.pximg.net/c/540x540_10_webp/img-master/img/2020/05/14/06/45/24/81547984_p0_square1200.jpg'
    )


def test_page_urls_in_post():
    assert pure.page_urls_in_post(page_illusts_new[22], size='medium') == ['https://i.pximg.net/c/540x540_70/img-master/img/2019/09/09/04/32/38/76695217_p0_master1200.jpg', 'https://i.pximg.net/c/540x540_70/img-master/img/2019/09/09/04/32/38/76695217_p1_master1200.jpg', 'https://i.pximg.net/c/540x540_70/img-master/img/2019/09/09/04/32/38/76695217_p2_master1200.jpg', 'https://i.pximg.net/c/540x540_70/img-master/img/2019/09/09/04/32/38/76695217_p3_master1200.jpg', 'https://i.pximg.net/c/540x540_70/img-master/img/2019/09/09/04/32/38/76695217_p4_master1200.jpg', 'https://i.pximg.net/c/540x540_70/img-master/img/2019/09/09/04/32/38/76695217_p5_master1200.jpg', 'https://i.pximg.net/c/540x540_70/img-master/img/2019/09/09/04/32/38/76695217_p6_master1200.jpg', 'https://i.pximg.net/c/540x540_70/img-master/img/2019/09/09/04/32/38/76695217_p7_master1200.jpg']
    assert len(pure.page_urls_in_post(page_illusts_new[22], size='medium')) == 8
    assert pure.page_urls_in_post(page_illusts_new[0], size='medium') == ['https://i.pximg.net/c/540x540_70/img-master/img/2020/05/14/06/45/24/81547984_p0_master1200.jpg']
    assert len(pure.page_urls_in_post(page_illusts_new[0], size='medium')) == 1


def test_post_titles_in_page():
    assert len(pure.post_titles_in_page(page_illusts_new)) == 30
    assert pure.post_titles_in_page(page_illusts_new)[0] == 'みこっちゃん'
    assert pure.post_titles_in_page(page_illusts_new)[1] == 'おりじなる'


def test_change_url_to_full():
    assert (
        pure.change_url_to_full('https://i.pximg.net/c/540x540_70/img-master/img/2019/09/09/04/32/38/76695217_p0_master1200.jpg')
        == 'https://i.pximg.net/c/540x540_70/img-master/img/2019/09/09/04/32/38/76695217_p0.jpg'
    )

def test_process_user_url():
    assert pure.process_user_url('https://www.pixiv.net/en/users/2232374') == '2232374'
    assert pure.process_user_url('2232374') == '2232374'

def test_process_artwork_url():
    assert (pure.process_artwork_url('https://www.pixiv.net/en/artworks/76695217') ==
            '76695217')
    assert pure.process_artwork_url('http://www.pixiv.net/member_illust.php?mode=medium&illust_id=76695217') == '76695217'
    assert pure.process_artwork_url('76695217') == '76695217'

def test_newnames_with_ext():
    assert pure.newnames_with_ext(
        ['www.example.com/image1.png', 'www.example.com/image2.png',
            'www.example.com/image3.png'],
        ['image1.png', 'image2.png', 'image3.png'],
        ['pic1', 'pic2', 'pic3']
    ) == ['000_pic1.png', '001_pic2.png', '002_pic3.png']


def test_xcoords():
    assert pure.xcoords(100) == [2, 20, 38, 56, 74]

def test_ycoords():
    assert pure.ycoords(20) == [0, 9]

def test_generate_orders():
    assert pure.generate_orders(120, 30) == [0, 30, 31, 32, 1, 33, 34, 35, 2, 36, 37, 38, 3, 39, 40, 41, 4, 42, 43, 44, 5, 45, 46, 47, 6, 48, 49, 50, 7, 51, 52, 53, 8, 54, 55, 56, 9, 57, 58, 59, 10, 60, 61, 62, 11, 63, 64, 65, 12, 66, 67, 68, 13, 69, 70, 71, 14, 72, 73, 74, 15, 75, 76, 77, 16, 78, 79, 80, 17, 81, 82, 83, 18, 84, 85, 86, 19, 87, 88, 89, 20, 90, 91, 92, 21, 93, 94, 95, 22, 96, 97, 98, 23, 99, 100, 101, 24, 102, 103, 104, 25, 105, 106, 107, 26, 108, 109, 110, 27, 111, 112, 113, 28, 114, 115, 116, 29, 117, 118, 119]


def test_line_width():
    assert pure.line_width(range(3), 5) == 8
