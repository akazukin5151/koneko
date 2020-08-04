import os
import sys
import random
from pathlib import Path
from unittest.mock import Mock, call
from collections import namedtuple

import pytest

from koneko import lscat, pure
from conftest import setup_test_config


FakeData = namedtuple('data', ('download_path',))


def test_icat(monkeypatch):
    mocked_pixcat = Mock()
    monkeypatch.setattr('koneko.lscat.Image', lambda *a, **k: mocked_pixcat)
    try:
        lscat.icat('./testing/files/004_祝！！！.jpg')
    except OSError:
        # Github doesn't connect to terminal
        return True
    assert mocked_pixcat.mock_calls == mocked_pixcat.method_calls == [call.show()]


def test_show_instant(monkeypatch, tmp_path, use_test_cfg_path):
    showed = []

    class FakeTracker:
        def __init__(self, data):
            pass

        def update(self, new):
            showed.append(new)

    FakeData = namedtuple('data', ('download_path',))

    # This config has print_info = True
    setup_test_config(tmp_path)

    fakedata = FakeData(Path('testing/files/'))
    lscat.show_instant(FakeTracker, fakedata)
    assert set(showed) == {
        '004_祝！！！.jpg', 'mode3.json',
        'mode1.json', 'not_an_image.txt', '008_77803142_p0.png',
        '017_ミコニャン.jpg', 'mode2.json'
    }


def test_TrackDownloads():
    mocked_data = Mock()
    mocked_generator = Mock()
    tracker = lscat.TrackDownloads(mocked_data)
    tracker.generator = mocked_generator

    correct_order = list(range(30))
    test_pics = [f"{str(idx).rjust(3, '0')}_test"
                 for idx in list(range(30))]

    # Shuffle list of pics
    for pic in random.sample(test_pics, 30):
        tracker.update(pic)

    assert len(mocked_generator.mock_calls) == 60 # 30 images * 2 because of None sends
    assert mocked_generator.mock_calls[:4] == [
        call.send('000_test'),
        call.send(None),
        call.send('001_test'),
        call.send(None),
    ]

    assert mocked_generator.mock_calls[-4:] == [
        call.send('028_test'),
        call.send(None),
        call.send('029_test'),
        call.send(None),
    ]


def test_TrackDownloadsUser():
    mocked_data = Mock()
    mocked_data.splitpoint = 30
    mocked_generator = Mock()
    tracker = lscat.TrackDownloadsUsers(mocked_data)
    tracker.generator = mocked_generator

    correct_order = pure.generate_orders(120, 30)
    test_pics = [f"{str(idx).rjust(3, '0')}_test"
                 for idx in list(range(120))]

    # Shuffle list of pics
    for pic in random.sample(test_pics, 120):
        tracker.update(pic)

    assert len(mocked_generator.mock_calls) == 240  # 120 images * 2 because of None sends
    assert mocked_generator.mock_calls[:8] == [
        call.send('000_test'),
        call.send(None),
        call.send('030_test'),
        call.send(None),
        call.send('031_test'),
        call.send(None),
        call.send('032_test'),
        call.send(None),
    ]
    assert mocked_generator.mock_calls[-8:] == [
        call.send('029_test'),
        call.send(None),
        call.send('117_test'),
        call.send(None),
        call.send('118_test'),
        call.send(None),
        call.send('119_test'),
        call.send(None),
    ]


def test_TrackDownloadsUser_with_koneko_file(tmp_path, use_test_cfg_path):
    """Test with .koneko file"""
    setup_test_config(tmp_path)

    data = FakeData(Path('testing/files/user'))
    data.download_path.mkdir()
    pics = ('004_祝！！！.jpg', '017_ミコニャン.jpg', '008_77803142_p0.png')
    for pic in pics:
        os.system(f'cp testing/files/{pic} testing/files/user/')

    os.system('touch testing/files/user/.koneko')
    with open('testing/files/user/.koneko', 'w') as f:
        f.write('3')

    lscat.show_instant(lscat.TrackDownloadsUsers, data)

    tracker = lscat.TrackDownloadsUsers(data)
    for pic in pics:
        tracker.update(pic)

    os.system(f'rm -r {data.download_path}')


def test_TrackDownloadsImage(monkeypatch):
    mocked_data = Mock()
    mocked_generator = Mock()
    mocked_data.page_num = 0
    tracker = lscat.TrackDownloadsImage(mocked_data)
    tracker.generator = mocked_generator

    correct_order = list(range(10))
    test_pics = [f"12345_p{idx}_master1200.jpg"
                 for idx in list(range(10))]

    # No need to shuffle because updates are always in order
    for pic in test_pics:
        tracker.update(pic)

    assert len(mocked_generator.mock_calls) == 20  # 10 images * 2 because of None sends
    assert mocked_generator.mock_calls[:4] == [
        call.send('12345_p0_master1200.jpg'),
        call.send(None),
        call.send('12345_p1_master1200.jpg'),
        call.send(None),
    ]
    assert mocked_generator.mock_calls[-4:] == [
        call.send('12345_p8_master1200.jpg'),
        call.send(None),
        call.send('12345_p9_master1200.jpg'),
        call.send(None),
    ]


def test_generate_page(monkeypatch, capsys):
    mocked_api = Mock()
    monkeypatch.setattr('koneko.lscat.api', mocked_api)
    monkeypatch.setattr('koneko.Terminal.width', 100)
    monkeypatch.setattr('koneko.Terminal.height', 20)
    monkeypatch.setattr('koneko.config.gallery_page_spacing_config', lambda: 1)
    monkeypatch.setattr('koneko.config.use_ueberzug', lambda: False)

    test_pics = [f"{str(idx).rjust(3, '0')}_test"
                 for idx in list(range(30))]

    gen = lscat.generate_page(Path('.'))  # Path doesn't matter
    gen.send(None)

    # No need to shuffle, tracker already shuffles
    for pic in test_pics:
        gen.send(pic)
        gen.send(None)

    assert mocked_api.mock_calls[:10] == [
        call.show(Path('000_test'), 2, 0, 310),
        call.show(Path('001_test'), 20, 0, 310),
        call.show(Path('002_test'), 38, 0, 310),
        call.show(Path('003_test'), 56, 0, 310),
        call.show(Path('004_test'), 74, 0, 310),
        call.show(Path('005_test'), 2, 9, 310),
        call.show(Path('006_test'), 20, 9, 310),
        call.show(Path('007_test'), 38, 9, 310),
        call.show(Path('008_test'), 56, 9, 310),
        call.show(Path('009_test'), 74, 9, 310)
    ]
    assert mocked_api.mock_calls[-10:] == [
        call.show(Path('020_test'), 2, 0, 310),
        call.show(Path('021_test'), 20, 0, 310),
        call.show(Path('022_test'), 38, 0, 310),
        call.show(Path('023_test'), 56, 0, 310),
        call.show(Path('024_test'), 74, 0, 310),
        call.show(Path('025_test'), 2, 9, 310),
        call.show(Path('026_test'), 20, 9, 310),
        call.show(Path('027_test'), 38, 9, 310),
        call.show(Path('028_test'), 56, 9, 310),
        call.show(Path('029_test'), 74, 9, 310)
    ]


def test_generate_users(monkeypatch, capsys):
    mocked_api = Mock()
    monkeypatch.setattr('koneko.lscat.api', mocked_api)
    monkeypatch.setattr('koneko.Terminal.width', 100)
    monkeypatch.setattr('koneko.Terminal.height', 20)
    monkeypatch.setattr('koneko.config.users_page_spacing_config', lambda: 1)
    monkeypatch.setattr('koneko.config.use_ueberzug', lambda: False)

    test_pics = [f"{str(idx).rjust(3, '0')}_test"
                 for idx in list(range(120))]

    gen = lscat.generate_users(Path('.'))  # Path doesn't matter
    gen.send(None)

    # No need to shuffle, tracker already shuffles
    for pic in test_pics:
        gen.send(pic)
        gen.send(None)

    assert mocked_api.mock_calls[:4] == [
        call.show(Path('000_test'), 2, 0, 310),
        call.show(Path('001_test'), 39, 0, 310),
        call.show(Path('002_test'), 57, 0, 310),
        call.show(Path('003_test'), 75, 0, 310),
    ]

    captured = capsys.readouterr()
    assert captured.out == '                  00\n                  test\n\n\n                  04\n                  test\n\n\n                  08\n                  test\n\n\n                  12\n                  test\n\n\n                  16\n                  test\n\n\n                  20\n                  test\n\n\n                  24\n                  test\n\n\n                  28\n                  test\n\n\n                  32\n                  test\n\n\n                  36\n                  test\n\n\n                  40\n                  test\n\n\n                  44\n                  test\n\n\n                  48\n                  test\n\n\n                  52\n                  test\n\n\n                  56\n                  test\n\n\n                  60\n                  test\n\n\n                  64\n                  test\n\n\n                  68\n                  test\n\n\n                  72\n                  test\n\n\n                  76\n                  test\n\n\n                  80\n                  test\n\n\n                  84\n                  test\n\n\n                  88\n                  test\n\n\n                  92\n                  test\n\n\n                  96\n                  test\n\n\n                  00\n                  test\n\n\n                  04\n                  test\n\n\n                  08\n                  test\n\n\n                  12\n                  test\n\n\n                  16\n                  test\n\n\n'


def test_generate_users_ueberzug(monkeypatch, capsys):
    mocked_api = Mock()
    monkeypatch.setattr('koneko.lscat.api', mocked_api)
    monkeypatch.setattr('koneko.Terminal.width', 100)
    monkeypatch.setattr('koneko.Terminal.height', 20)
    monkeypatch.setattr('koneko.config.users_page_spacing_config', lambda: 1)
    monkeypatch.setattr('koneko.config.use_ueberzug', lambda: True)

    test_pics = [f"{str(idx).rjust(3, '0')}_test"
                 for idx in list(range(120))]

    gen = lscat.generate_users_ueberzug(Path('.'))  # Path doesn't matter
    gen.send(None)

    # No need to shuffle, tracker already shuffles
    for pic in test_pics:
        gen.send(pic)
        gen.send(None)

    assert mocked_api.mock_calls == [
        call.show(Path('000_test'), 2, 0, 310),
        call.show(Path('001_test'), 39, 0, 310),
        call.show(Path('002_test'), 57, 0, 310),
        call.show(Path('003_test'), 75, 0, 310),
        call.show(Path('004_test'), 2, 9, 310),
        call.show(Path('005_test'), 39, 9, 310),
        call.show(Path('006_test'), 57, 9, 310),
        call.show(Path('007_test'), 75, 9, 310)
    ]

    captured = capsys.readouterr()
    assert captured.out == '                  00\n                  test\n\n\n\n\n\n\n\n\n\n                  04\n                  test\n'


def test_generate_previews_ueberzug(monkeypatch):
    monkeypatch.setattr('koneko.config.use_ueberzug', lambda: True)
    mocked_api = Mock()
    monkeypatch.setattr('koneko.lscat.api', mocked_api)
    monkeypatch.setattr('koneko.Terminal.width', 100)
    monkeypatch.setattr('koneko.Terminal.height', 20)

    test_pics = [f"12345_p{idx}_master1200.jpg"
                 for idx in list(range(10))]

    gen = lscat.generate_previews_ueberzug(Path('.'), 10)  # Path doesn't matter
    gen.send(None)

    # No need to shuffle, tracker already shuffles
    for pic in test_pics:
        gen.send(pic)
        gen.send(None)

    assert mocked_api.mock_calls == [
        call.show(Path('12345_p0_master1200.jpg'), 2, 0, 310),
        call.show(Path('12345_p1_master1200.jpg'), 2, 9, 310),
        call.show(Path('12345_p2_master1200.jpg'), 74, 0, 310),
        call.show(Path('12345_p3_master1200.jpg'), 74, 9, 310),
    ]
