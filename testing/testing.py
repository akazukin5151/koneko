import os
import json
from pathlib import Path

import pytest

from koneko import pure, lscat, utils, data, KONEKODIR
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
    assert pure.process_artwork_url("76695217") == ("76695217", "2")

# From lscat.py
def test_is_image():
    assert lscat.is_image("testing/04_祝！！！.jpg") == True
    assert lscat.is_image("testing/17_ミコニャン.jpg") == True
    assert lscat.is_image("testing/77803142_p0.png") == True
    assert lscat.is_image("testing/not_an_image.txt") == False


def test_filter_img():
    # Run in main koneko dir
    assert lscat.filter_img("testing/") == [
        "04_祝！！！.jpg",
        "17_ミコニャン.jpg",
        "77803142_p0.png",
    ]

mywidth = 90 // 5  # == 18


def test_xcoord():
    assert lscat.xcoord(1, 5, mywidth) == 20


def test_number_prefix():
    assert lscat.number_prefix("02_file.png") == 2
    assert lscat.number_prefix("11_file.png") == 11

def test_xcoords():
    assert lscat.xcoords(100) == [2, 20, 38, 56, 74]

def test_icat():
    assert lscat.icat("testing/04_祝！！！.jpg") == None


# From data.py
with open('testing/mode1.json', 'r') as json_file:
    mode1 = json.load(json_file)
with open('testing/mode2.json', 'r') as json_file:
    mode2 = json.load(json_file)
with open('testing/mode3.json', 'r') as json_file:
    mode3 = json.load(json_file)


def test_gallery():
    gdata = data.GalleryJson(1, Path(f"{KONEKODIR}/2232374/"))
    assert gdata
    assert gdata.all_pages_cache == {}
    assert gdata.current_page_num == 1
    assert gdata._main_path == Path(f"{KONEKODIR}/2232374/")

    gdata.update(mode1)
    assert list(gdata.all_pages_cache.keys()) == ['1']
    assert gdata.all_pages_cache["1"] == mode1

    assert gdata.download_path == Path(f"{KONEKODIR}/2232374/1")

    assert len(gdata.current_illusts) == 30

    assert gdata.post_json(0) == json.loads("""{"id":81547984,"title":"みこっちゃん","type":"illust","image_urls":{"square_medium":"https://i.pximg.net/c/540x540_10_webp/img-master/img/2020/05/14/06/45/24/81547984_p0_square1200.jpg","medium":"https://i.pximg.net/c/540x540_70/img-master/img/2020/05/14/06/45/24/81547984_p0_master1200.jpg","large":"https://i.pximg.net/c/600x1200_90_webp/img-master/img/2020/05/14/06/45/24/81547984_p0_master1200.jpg"},"caption":"( ˘ω˘ )ﾃﾞｽ","restrict":0,"user":{"id":2232374,"name":"raika9","account":"raika9","profile_image_urls":{"medium":"https://i.pximg.net/user-profile/img/2016/06/30/03/20/52/11132477_4b836884eae72b4e90061719fd75180b_170.jpg"},"is_followed":true},"tags":[{"name":"とある科学の超電磁砲","translated_name":null},{"name":"とある魔術の禁書目録","translated_name":null},{"name":"御坂美琴","translated_name":null}],"tools":["CLIP STUDIO PAINT"],"create_date":"2020-05-14T06:45:24+09:00","page_count":1,"width":764,"height":1087,"sanity_level":2,"x_restrict":0,"series":null,"meta_single_page":{"original_image_url":"https://i.pximg.net/img-original/img/2020/05/14/06/45/24/81547984_p0.jpg"},"meta_pages":[],"total_view":8021,"total_bookmarks":2324,"is_bookmarked":false,"visible":true,"is_muted":false,"total_comments":54}""")

    assert gdata.artist_user_id(0) == 2232374

    assert gdata.image_id(0) == 81547984

    assert list(gdata.cached_pages) == ["1"]

    assert gdata.next_url == "https://app-api.pixiv.net/v1/user/illusts?user_id=2232374&filter=for_ios&type=illust&offset=30"

    assert gdata.first_img == "みこっちゃん"

    assert gdata.page_num == 1
    assert gdata.page_num == gdata.current_page_num


def test_image():
    idata = data.ImageJson(mode2["illust"], "76695217")
    assert idata
    assert idata.url == "https://i.pximg.net/c/600x1200_90_webp/img-master/img/2019/09/09/04/32/38/76695217_p0_master1200.jpg"
    assert idata.filename == "76695217_p0_master1200.jpg"
    assert idata.artist_user_id == 2232374
    assert idata.page_num == 0
    assert idata.number_of_pages == 8
    assert idata.page_urls == ["https://i.pximg.net/c/600x1200_90_webp/img-master/img/2019/09/09/04/32/38/76695217_p0_master1200.jpg", "https://i.pximg.net/c/600x1200_90_webp/img-master/img/2019/09/09/04/32/38/76695217_p1_master1200.jpg", "https://i.pximg.net/c/600x1200_90_webp/img-master/img/2019/09/09/04/32/38/76695217_p2_master1200.jpg", "https://i.pximg.net/c/600x1200_90_webp/img-master/img/2019/09/09/04/32/38/76695217_p3_master1200.jpg", "https://i.pximg.net/c/600x1200_90_webp/img-master/img/2019/09/09/04/32/38/76695217_p4_master1200.jpg", "https://i.pximg.net/c/600x1200_90_webp/img-master/img/2019/09/09/04/32/38/76695217_p5_master1200.jpg", "https://i.pximg.net/c/600x1200_90_webp/img-master/img/2019/09/09/04/32/38/76695217_p6_master1200.jpg", "https://i.pximg.net/c/600x1200_90_webp/img-master/img/2019/09/09/04/32/38/76695217_p7_master1200.jpg"]
    assert idata.downloaded_images == ["76695217_p0_master1200.jpg", "76695217_p1_master1200.jpg"]
    assert idata.large_dir == Path(f"{KONEKODIR}/2232374/individual/76695217/")

    assert idata.image_filename == "76695217_p0_master1200.jpg"

    assert idata.filepath == Path(f"{KONEKODIR}/2232374/individual/76695217/76695217_p0_master1200.jpg")

    assert idata.next_img_url == "https://i.pximg.net/c/600x1200_90_webp/img-master/img/2019/09/09/04/32/38/76695217_p1_master1200.jpg"

    assert idata.current_url == "https://i.pximg.net/c/600x1200_90_webp/img-master/img/2019/09/09/04/32/38/76695217_p0_master1200.jpg"


def test_user():
    udata = data.UserJson(mode3, 1, Path(f"{KONEKODIR}/following/"), "2232374")
    assert udata

    assert udata.page_num == 1
    assert udata.main_path == Path(f"{KONEKODIR}/following/")
    assert udata._input == "2232374"

    assert udata.next_url == "https://app-api.pixiv.net/v1/user/following?user_id=2232374&restrict=private&offset=30"
    assert udata.ids_cache == {1: [219621, 1510169, 12612404, 8660134, 15063, 28245700, 33137265, 2702224, 24218478, 625051, 95391, 9427, 1193008, 1554775, 11103, 7309825, 5301174, 4316556, 10573236, 29362997, 809099, 82688, 15608555, 30803054, 18836733, 644670, 2397243, 14211481, 8092144, 8175661]}
    assert udata.names_cache == {1: ["畳と桧", "ざるつ", "春夫", "JAM", "肋兵器", "おてん!!!!!!!!", "saber", "sola7764", "￦ANKE", "ToY", "sigma99", "アマガイタロー", "望月けい", "米山舞", "にえあ@冬コミ新刊委託中です", "白萝炖黑兔", "Kelinch1", "三崎二式.N3", "ﾕｳｷ", "sunhyunそんひょん선현", "うまくち醤油", "Prime", "哦雅思密乃", "ホリセイ", "pattsk138", "DELF", "キンタ", "cookies", "Aluppia", "うにゃりすたー"]}
    assert len(udata.profile_pic_urls) == 30
    assert len(udata.image_urls) == 87

    assert udata.download_path == Path(f"{KONEKODIR}/following/2232374/1")

    assert udata.artist_user_id(0) == 219621

    assert udata.names == udata.names_cache[1]

    assert udata.names_prefixed == ["00\n                   畳と桧", "01\n                   ざるつ", "02\n                   春夫", "03\n                   JAM", "04\n                   肋兵器", "05\n                   おてん!!!!!!!!", "06\n                   saber", "07\n                   sola7764", "08\n                   ￦ANKE", "09\n                   ToY", "10\n                   sigma99", "11\n                   アマガイタロー", "12\n                   望月けい", "13\n                   米山舞", "14\n                   にえあ@冬コミ新刊委託中です", "15\n                   白萝炖黑兔", "16\n                   Kelinch1", "17\n                   三崎二式.N3", "18\n                   ﾕｳｷ", "19\n                   sunhyunそんひょん선현", "20\n                   うまくち醤油", "21\n                   Prime", "22\n                   哦雅思密乃", "23\n                   ホリセイ", "24\n                   pattsk138", "25\n                   DELF", "26\n                   キンタ", "27\n                   cookies", "28\n                   Aluppia", "29\n                   うにゃりすたー"]

    assert len(udata.all_urls) == 117

    assert udata.all_names == ["畳と桧", "ざるつ", "春夫", "JAM", "肋兵器", "おてん!!!!!!!!", "saber", "sola7764", "￦ANKE", "ToY", "sigma99", "アマガイタロー", "望月けい", "米山舞", "にえあ@冬コミ新刊委託中です", "白萝炖黑兔", "Kelinch1", "三崎二式.N3", "ﾕｳｷ", "sunhyunそんひょん선현", "うまくち醤油", "Prime", "哦雅思密乃", "ホリセイ", "pattsk138", "DELF", "キンタ", "cookies", "Aluppia", "うにゃりすたー", "81258221_p0_square1200", "81035530_p0_square1200", "80699629_p0_square1200", "78613317_p0_square1200", "74541047_p0_square1200", "72512314_p0_square1200", "78113353_p0_square1200", "77309367_p0_square1200", "77305517_p0_square1200", "80956202_p0_square1200", "79573047_p0_square1200", "79335561_p0_square1200", "81422477_p0_square1200", "80555468_p0_square1200", "79145744_p0_square1200", "80987017_p0_square1200", "80904535_p0_square1200", "80899986_p0_square1200", "79603234_p0_square1200", "79530224_p0_square1200", "78581098_p0_square1200", "81363682_p0_square1200", "81270363_p0_square1200", "81213886_p0_square1200", "50246878_p0_square1200", "50223367_p0_square1200", "43231172_p0_square1200", "77180657_p0_square1200", "74967427_p0_square1200", "74967394_p0_square1200", "78446413_p0_square1200", "75034396_p0_square1200", "66806885_p0_square1200", "80900186_p0_square1200", "80680801_p0_square1200", "80423566_p0_square1200", "80786439_p0_square1200", "79342948_p0_square1200", "78830672_p0_square1200", "81108823_p0_square1200", "80457352_p0_square1200", "78430684_p0_square1200", "81609513_p0_square1200", "81541593_p0_square1200", "81459961_p0_square1200", "77499932_p0_square1200", "76790778_p0_square1200", "75998778_p0_square1200", "81664872_p0_square1200", "81603032_p0_square1200", "81602973_p0_square1200", "78590221_p0_square1200", "78474483_p0_square1200", "76363757_p0_square1200", "81241007_p0_square1200", "81011738_p0_square1200", "80722852_p0_square1200", "81150911_p0_square1200", "80717346_p0_square1200", "79429474_p0_square1200", "76043556_p0_square1200", "75936667_p0_square1200", "69956757_p0_square1200", "78013455_p0_square1200", "66209081_p0_square1200", "53810308_p0_square1200", "81287716_p0_square1200", "80837899_p0_square1200", "79198662_p0_square1200", "76344408_p0_square1200", "76101063_p0_square1200", "76036041_p0_square1200", "78920807_p0_square1200", "76118660_p0_square1200", "76115853_p0_square1200", "80492957_p0_square1200", "77970884_p0_square1200", "76547709_p0_square1200", "79708221_p0_square1200", "76623178_p0_square1200", "74653820_p0_square1200", "81542404_p0_square1200", "80414334_p0_square1200", "79663557_p0_square1200", "79028150_p0_square1200", "79027961_p0_square1200", "79027291_p0_square1200"]

    assert udata.splitpoint == 30

    assert udata.first_img == "畳と桧"


# utils.py
def test_verify_full_download():
    assert utils.verify_full_download("testing/77803142_p0.png") == True
    assert utils.verify_full_download("testing/not_an_image.txt") == False
    # The above code will remove the file
    os.system("touch testing/not_an_image.txt")

def test_show_artist_illusts(monkeypatch):
    # Apparently monkeypatching the lscat function needs to start with
    # the name of the current module...
    monkeypatch.setattr("testing.lscat.Gallery.render", lambda x: "")
    utils.show_artist_illusts(Path(f"{KONEKODIR}/2232374/1"))

def test_begin_prompt(monkeypatch):
    # Send a "1" as input
    monkeypatch.setattr("builtins.input", lambda x: "1")
    returned = utils.begin_prompt()
    assert returned == "1"
    os.system("kitty +kitten icat --clear")

def test_artist_user_id_prompt(monkeypatch):
    monkeypatch.setattr("builtins.input",
                        lambda x: "https://www.pixiv.net/en/users/2232374")
    myinput = utils.artist_user_id_prompt()
    assert myinput == "https://www.pixiv.net/en/users/2232374"

def test_show_man_loop(monkeypatch):
    monkeypatch.setattr("builtins.input", lambda x: "")
    monkeypatch.setattr("builtins.print", lambda *a, **k: "")
    monkeypatch.setattr("os.system", lambda x: "")
    utils.show_man_loop()
    os.system("kitty +kitten icat --clear")

def test_clear_cache_loop(monkeypatch):
    monkeypatch.setattr("shutil.rmtree", lambda x: "")
    monkeypatch.setattr("builtins.input", lambda x: "y")
    monkeypatch.setattr("builtins.print", lambda *a, **k: "")
    monkeypatch.setattr("os.system", lambda x: "")
    utils.clear_cache_loop()
    monkeypatch.setattr("builtins.input", lambda x: "n")
    utils.clear_cache_loop()

def test_info_screen_loop(monkeypatch):
    monkeypatch.setattr("builtins.input", lambda x: "")
    monkeypatch.setattr("os.system", lambda x: "")
    utils.info_screen_loop()
    os.system("kitty +kitten icat --clear")

