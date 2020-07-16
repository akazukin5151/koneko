import sys
import json
from pathlib import Path

from koneko import KONEKODIR, data

# Lmao python
sys.path.append('testing')

with open('testing/files/mode1.json', 'r') as json_file:
    mode1 = json.load(json_file)
with open('testing/files/mode2.json', 'r') as json_file:
    mode2 = json.load(json_file)
with open('testing/files/mode3.json', 'r') as json_file:
    mode3 = json.load(json_file)


def gallery():
    return data.GalleryData(1, KONEKODIR / "2232374")

def gallery_updated():
    data = gallery()
    data.update(mode1)
    return data

def image():
    return data.ImageData(mode2["illust"], "76695217")

def user():
    return data.UserData(1, KONEKODIR / "following/2232374")

def user_updated():
    data = user()
    data.update(mode3)
    return data


def test_urls_as_names_gdata():
    gdata = gallery()
    gdata.update(mode1)
    assert gdata.urls_as_names == ['81547984_p0_square1200.jpg','81501385_p0_square1200.jpg','81468125_p0_square1200.jpg','81416496_p0_square1200.jpg','81368866_p0_square1200.jpg','81276257_p0_square1200.jpg','80923496_p0_square1200.jpg','80701898_p0_square1200.jpg','80017594_p0_square1200.jpg','79799236_p0_square1200.jpg','79658392_p0_square1200.jpg','79549991_p0_square1200.jpg','78823485_p0_square1200.jpg','78628383_p0_square1200.jpg','78403815_p0_square1200.jpg','78378594_p0_square1200.jpg','78201587_p0_square1200.jpg','77804404_p0_square1200.jpg','77565309_p0_square1200.jpg','77460464_p0_square1200.jpg','77347697_p0_square1200.jpg','77068750_p0_square1200.jpg','76695217_p0_square1200.jpg','76561671_p0_square1200.jpg','76138362_p0_square1200.jpg','75933779_p0_square1200.jpg','75810852_p0_square1200.jpg','75698678_p0_square1200.jpg','75579060_p0_square1200.jpg','75457783_p0_square1200.jpg']

def test_urls_as_names_udata():
    udata = user()
    udata.update(mode3)
    assert udata.urls_as_names[:10] == ['14320305_16783de875f6e2bbb9d22a70befca4f2_170.jpg','13690256_d2a74f1a7527fc96da018ad17c0f9ed8_170.jpg','8614973_404eb78e066872d9b8a558360c752b01_170.png','16393889_c172be60e91a79ca65580f945a8b7860_170.jpg','999552_ca806e5e217f411d11c1b172180ffe26_170.jpg','no_profile.png','no_profile.png','7954023_170fa2e3007d8e7764f1d09e113c652c_170.jpg','16445257_404ce224320f5dac49b6715fafd3824d_170.jpg','3459330_6c425ecb3ba82c77a99e4fd74df42fec_170.jpg',]
    assert udata.urls_as_names[-10:] == ['76547709_p0_square1200.jpg','79708221_p0_square1200.jpg','76623178_p0_square1200.jpg','74653820_p0_square1200.jpg','81542404_p0_square1200.jpg','80414334_p0_square1200.jpg','79663557_p0_square1200.jpg','79028150_p0_square1200.jpg','79027961_p0_square1200.jpg','79027291_p0_square1200.jpg']

def test_newnames_with_ext_gdata():
    gdata = gallery()
    gdata.update(mode1)
    assert gdata.newnames_with_ext  == ['000_みこっちゃん.jpg','001_おりじなる.jpg','002_0510.jpg','003_5.3.jpg','004_おりじなる.jpg','005_ミコ誕オメ画！.jpg','006_5.2.jpg','007_5.1.jpg','008_310.jpg','009_Midnight Sun.jpg','010_222.jpg','011_バレンタイン.jpg','012_祝！！！.jpg','013_あけましておめでとうございます.jpg','014_ミコサンタ.jpg','015_C97告知.jpg','016_ミコバニー.jpg','017_たちかわ楽市2019仕様4人組.jpg','018_ハロミコ.jpg','019_夏服.jpg','020_御坂美琴写真集１０用.jpg','021_常盤台中学指定体操服改.jpg','022_ツイッターまとめ.jpg','023_スクミズミコクロ.jpg','024_ミズミコ.jpg','025_ミコニャン.jpg','026_とある画帖.jpg','027_御坂美琴写真集９.jpg','028_ジャンプ！.jpg','029_シャワミコ.jpg']

def test_newnames_with_ext_udata():
    udata = user()
    udata.update(mode3)
    assert udata.newnames_with_ext[:10] == ['000_畳と桧.jpg','001_ざるつ.jpg','002_春夫.png','003_JAM.jpg','004_肋兵器.jpg','005_おてん!!!!!!!!.png','006_saber.png','007_sola7764.jpg','008_￦ANKE.jpg','009_ToY.jpg']
    assert udata.newnames_with_ext[-10:] == ['107_76547709_p0_square1200.jpg','108_79708221_p0_square1200.jpg','109_76623178_p0_square1200.jpg','110_74653820_p0_square1200.jpg','111_81542404_p0_square1200.jpg','112_80414334_p0_square1200.jpg','113_79663557_p0_square1200.jpg','114_79028150_p0_square1200.jpg','115_79027961_p0_square1200.jpg','116_79027291_p0_square1200.jpg']


def test_gallery_init():
    gdata = gallery()
    assert gdata
    assert gdata.all_pages_cache == {}
    assert gdata.page_num == 1
    assert gdata.main_path == Path(f"{KONEKODIR}/2232374/")

def test_gallery_update():
    gdata = gallery()
    gdata.update(mode1)
    assert list(gdata.all_pages_cache.keys()) == ['1']
    assert gdata.all_pages_cache["1"] == mode1

def test_gallery_download_path():
    gdata = gallery_updated()
    assert gdata.download_path == KONEKODIR / "2232374" / '1'

def test_gallery_current_illusts():
    gdata = gallery_updated()
    assert len(gdata.current_illusts) == 30

def test_gallery_post_json():
    gdata = gallery_updated()
    assert gdata.post_json(0) == json.loads("""{"id":81547984,"title":"みこっちゃん","type":"illust","image_urls":{"square_medium":"https://i.pximg.net/c/540x540_10_webp/img-master/img/2020/05/14/06/45/24/81547984_p0_square1200.jpg","medium":"https://i.pximg.net/c/540x540_70/img-master/img/2020/05/14/06/45/24/81547984_p0_master1200.jpg","large":"https://i.pximg.net/c/600x1200_90_webp/img-master/img/2020/05/14/06/45/24/81547984_p0_master1200.jpg"},"caption":"( ˘ω˘ )ﾃﾞｽ","restrict":0,"user":{"id":2232374,"name":"raika9","account":"raika9","profile_image_urls":{"medium":"https://i.pximg.net/user-profile/img/2016/06/30/03/20/52/11132477_4b836884eae72b4e90061719fd75180b_170.jpg"},"is_followed":true},"tags":[{"name":"とある科学の超電磁砲","translated_name":null},{"name":"とある魔術の禁書目録","translated_name":null},{"name":"御坂美琴","translated_name":null}],"tools":["CLIP STUDIO PAINT"],"create_date":"2020-05-14T06:45:24+09:00","page_count":1,"width":764,"height":1087,"sanity_level":2,"x_restrict":0,"series":null,"meta_single_page":{"original_image_url":"https://i.pximg.net/img-original/img/2020/05/14/06/45/24/81547984_p0.jpg"},"meta_pages":[],"total_view":8021,"total_bookmarks":2324,"is_bookmarked":false,"visible":true,"is_muted":false,"total_comments":54}""")

def test_gallery_artist_user_id():
    gdata = gallery_updated()
    assert gdata.artist_user_id(0) == 2232374

def test_gallery_image_id():
    gdata = gallery_updated()
    assert gdata.image_id(0) == 81547984

def test_gallery_next_url():
    gdata = gallery_updated()
    assert gdata.next_url == "https://app-api.pixiv.net/v1/user/illusts?user_id=2232374&filter=for_ios&type=illust&offset=30"

def test_gallery_url():
    gdata = gallery_updated()
    assert gdata.url(0) == 'https://i.pximg.net/c/600x1200_90_webp/img-master/img/2020/05/14/06/45/24/81547984_p0_master1200.jpg'

def test_gallery_all_urls():
    gdata = gallery_updated()
    assert gdata.all_urls[:3] == ['https://i.pximg.net/c/540x540_10_webp/img-master/img/2020/05/14/06/45/24/81547984_p0_square1200.jpg', 'https://i.pximg.net/c/540x540_10_webp/img-master/img/2020/05/12/06/36/27/81501385_p0_square1200.jpg', 'https://i.pximg.net/c/540x540_10_webp/img-master/img/2020/05/10/23/10/38/81468125_p0_square1200.jpg']

def test_gallery_all_names():
    gdata = gallery_updated()
    assert gdata.all_names == ['みこっちゃん', 'おりじなる', '0510', '5.3', 'おりじなる', 'ミコ誕オメ画！', '5.2', '5.1', '310', 'Midnight Sun', '222', 'バレンタイン', '祝！！！', 'あけましておめでとうございます', 'ミコサンタ', 'C97告知', 'ミコバニー', 'たちかわ楽市2019仕様4人組', 'ハロミコ', '夏服', '御坂美琴写真集１０用', '常盤台中学指定体操服改', 'ツイッターまとめ', 'スクミズミコクロ', 'ミズミコ', 'ミコニャン', 'とある画帖', '御坂美琴写真集９', 'ジャンプ！', 'シャワミコ']


def test_image_artist_user_id():
    idata = image()
    assert idata.artist_user_id == 2232374

def test_image_page_num():
    idata = image()
    assert idata.page_num == 0

def test_image_number_of_pages():
    idata = image()
    assert idata.number_of_pages == 8

def test_image_page_urls():
    idata = image()
    assert idata.page_urls == ["https://i.pximg.net/c/600x1200_90_webp/img-master/img/2019/09/09/04/32/38/76695217_p0_master1200.jpg", "https://i.pximg.net/c/600x1200_90_webp/img-master/img/2019/09/09/04/32/38/76695217_p1_master1200.jpg", "https://i.pximg.net/c/600x1200_90_webp/img-master/img/2019/09/09/04/32/38/76695217_p2_master1200.jpg", "https://i.pximg.net/c/600x1200_90_webp/img-master/img/2019/09/09/04/32/38/76695217_p3_master1200.jpg", "https://i.pximg.net/c/600x1200_90_webp/img-master/img/2019/09/09/04/32/38/76695217_p4_master1200.jpg", "https://i.pximg.net/c/600x1200_90_webp/img-master/img/2019/09/09/04/32/38/76695217_p5_master1200.jpg", "https://i.pximg.net/c/600x1200_90_webp/img-master/img/2019/09/09/04/32/38/76695217_p6_master1200.jpg", "https://i.pximg.net/c/600x1200_90_webp/img-master/img/2019/09/09/04/32/38/76695217_p7_master1200.jpg"]

def test_image_download_path():
    idata = image()
    assert idata.download_path == Path(f"{KONEKODIR}/2232374/individual/76695217/")

def test_image_image_filename():
    idata = image()
    assert idata.image_filename == "76695217_p0_master1200.jpg"

def test_image_filepath():
    idata = image()
    assert idata.filepath == Path(f"{KONEKODIR}/2232374/individual/76695217/76695217_p0_master1200.jpg")

def test_image_next_img_url():
    idata = image()
    assert idata.next_img_url == "https://i.pximg.net/c/600x1200_90_webp/img-master/img/2019/09/09/04/32/38/76695217_p1_master1200.jpg"

def test_image_current_url():
    idata = image()
    assert idata.current_url == "https://i.pximg.net/c/600x1200_90_webp/img-master/img/2019/09/09/04/32/38/76695217_p0_master1200.jpg"


def test_user_init():
    udata = user()
    assert udata.page_num == 1
    assert udata.main_path == KONEKODIR / 'following' / '2232374'
    assert udata.ids_cache == udata.names_cache == {}

def test_user_update():
    udata = user()
    udata.update(mode3)
    assert udata.next_url == "https://app-api.pixiv.net/v1/user/following?user_id=2232374&restrict=private&offset=30"

    assert udata.ids_cache == {1: [219621, 1510169, 12612404, 8660134, 15063, 28245700, 33137265, 2702224, 24218478, 625051, 95391, 9427, 1193008, 1554775, 11103, 7309825, 5301174, 4316556, 10573236, 29362997, 809099, 82688, 15608555, 30803054, 18836733, 644670, 2397243, 14211481, 8092144, 8175661]}

    assert udata.names_cache == {1: ["畳と桧", "ざるつ", "春夫", "JAM", "肋兵器", "おてん!!!!!!!!", "saber", "sola7764", "￦ANKE", "ToY", "sigma99", "アマガイタロー", "望月けい", "米山舞", "にえあ@冬コミ新刊委託中です", "白萝炖黑兔", "Kelinch1", "三崎二式.N3", "ﾕｳｷ", "sunhyunそんひょん선현", "うまくち醤油", "Prime", "哦雅思密乃", "ホリセイ", "pattsk138", "DELF", "キンタ", "cookies", "Aluppia", "うにゃりすたー"]}

    assert len(udata.profile_pic_urls) == 30

    assert len(udata.image_urls) == 87

def test_user_download_path():
    udata = user_updated()
    assert udata.download_path == KONEKODIR / "following/2232374/1"

def test_user_artist_user_id():
    udata = user_updated()
    assert udata.artist_user_id(0) == 219621

def test_user_names():
    udata = user_updated()
    assert udata.names == udata.names_cache[1]

def test_user_all_urls():
    udata = user_updated()
    assert len(udata.all_urls) == 117

def test_user_all_names():
    udata = user_updated()
    assert udata.all_names[:10] == ["畳と桧", "ざるつ", "春夫", "JAM", "肋兵器", "おてん!!!!!!!!", "saber", "sola7764", "￦ANKE", "ToY"]
    assert udata.all_names[-10:] == ["76547709_p0_square1200", "79708221_p0_square1200", "76623178_p0_square1200", "74653820_p0_square1200", "81542404_p0_square1200", "80414334_p0_square1200", "79663557_p0_square1200", "79028150_p0_square1200", "79027961_p0_square1200", "79027291_p0_square1200"]

def test_user_splitpoint():
    udata = user_updated()
    assert udata.splitpoint == 30

