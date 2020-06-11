import os
import sys
import random
from pathlib import Path
from unittest.mock import Mock, call

import pytest

from koneko import lscat

# Lmao python
sys.path.append('testing')


def test_xcoords():
    assert lscat.xcoords(100) == [2, 20, 38, 56, 74]

def test_ycoords():
    assert lscat.ycoords(20) == [0, 9]

def test_icat():
    assert lscat.icat("testing/files/04_祝！！！.jpg") == None

def test_show_instant(monkeypatch):
    showed = []

    class FakeTracker:
        def __init__(self, data):
            pass

        def update(self, new):
            showed.append(new)

    class FakeData:
        def __init__(self):
            self.download_path = Path('testing/files/')

    # This config has print_cols = True
    # Can't use shared fixture because pathlib is used in FakeData
    monkeypatch.setattr('koneko.utils.Path.expanduser',
                        lambda x: Path('testing/test_config.ini'))

    fakedata = FakeData()
    lscat.show_instant(FakeTracker, fakedata, True)
    # First one works for me, but second one works for github
    assert set(showed) == set([
        'test_config.ini', '004_祝！！！.jpg', 'mode3.json',
        'mode1.json', 'not_an_image.txt', '008_77803142_p0.png',
        '017_ミコニャン.jpg', 'mode2.json'
    ]) or set(showed) == set([
        '004_祝！！！.jpg', 'mode3.json',
        'mode1.json', 'not_an_image.txt', '008_77803142_p0.png',
        '017_ミコニャン.jpg', 'mode2.json'
    ])


def test_generate_orders():
    assert lscat.generate_orders(120, 30) == [0, 30, 31, 32, 1, 33, 34, 35, 2, 36, 37, 38, 3, 39, 40, 41, 4, 42, 43, 44, 5, 45, 46, 47, 6, 48, 49, 50, 7, 51, 52, 53, 8, 54, 55, 56, 9, 57, 58, 59, 10, 60, 61, 62, 11, 63, 64, 65, 12, 66, 67, 68, 13, 69, 70, 71, 14, 72, 73, 74, 15, 75, 76, 77, 16, 78, 79, 80, 17, 81, 82, 83, 18, 84, 85, 86, 19, 87, 88, 89, 20, 90, 91, 92, 21, 93, 94, 95, 22, 96, 97, 98, 23, 99, 100, 101, 24, 102, 103, 104, 25, 105, 106, 107, 26, 108, 109, 110, 27, 111, 112, 113, 28, 114, 115, 116, 29, 117, 118, 119]


def test_TrackDownloads(monkeypatch):
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

    assert len(mocked_generator.mock_calls) == 30
    methods_called = [mocked_generator.mock_calls[i][0] for i in range(30)]
    # Only thing the tracker does is to call send() on the generator
    assert methods_called == ['send'] * 30

    #         first value in tuple (eg '020_test') <--|
    #                                                 |  |--> convert digits to int
    #  arg passed into generator.send, is tuple <--|  |  |
    #                                              |  |  |
    sent_img = [int(mocked_generator.mock_calls[i][1][0][:3]) for i in range(30)]
    assert correct_order == sent_img


def test_TrackDownloadsUser(monkeypatch):
    mocked_data = Mock()
    mocked_data.splitpoint = 30
    mocked_generator = Mock()
    tracker = lscat.TrackDownloadsUsers(mocked_data)
    tracker.generator = mocked_generator

    correct_order = lscat.generate_orders(120, 30)
    test_pics = [f"{str(idx).rjust(3, '0')}_test"
                 for idx in list(range(120))]

    # Shuffle list of pics
    for pic in random.sample(test_pics, 120):
        tracker.update(pic)

    assert len(mocked_generator.mock_calls) == 120
    methods_called = [mocked_generator.mock_calls[i][0] for i in range(120)]
    # Only thing the tracker does is to call send() on the generator
    assert methods_called == ['send'] * 120

    #         first value in tuple (eg '020_test') <--|
    #                                                 |  |--> convert digits to int
    #  arg passed into generator.send, is tuple <--|  |  |
    #                                              |  |  |
    sent_img = [int(mocked_generator.mock_calls[i][1][0][:3]) for i in range(120)]
    assert correct_order == sent_img


def test_TrackDownloadsUser2(monkeypatch):
    """Test with .koneko file"""
    class FakeData:
        def __init__(self):
            self.download_path = Path('testing/files/user')

    # Can't use shared fixture because pathlib is used in FakeData
    monkeypatch.setattr('koneko.utils.Path.expanduser',
                        lambda x: Path('testing/test_config.ini'))

    data = FakeData()
    data.download_path.mkdir()
    pics = ('004_祝！！！.jpg', '017_ミコニャン.jpg', '008_77803142_p0.png')
    for pic in pics:
        os.system(f'cp testing/files/{pic} testing/files/user/')

    os.system('touch testing/files/user/.koneko')
    with open('testing/files/user/.koneko', 'w') as f:
        f.write("3")

    lscat.show_instant(lscat.TrackDownloadsUsers, data)

    tracker = lscat.TrackDownloadsUsers(data)
    for pic in pics:
        tracker.update(pic)

    os.system(f'rm -r {data.download_path}')

def test_generate_page(monkeypatch):
    mocked_pixcat = Mock()
    monkeypatch.setattr("koneko.lscat.Image", lambda *a, **k: mocked_pixcat)
    monkeypatch.setattr("koneko.lscat.Terminal.width", 100)
    monkeypatch.setattr("koneko.lscat.Terminal.height", 20)

    test_pics = [f"{str(idx).rjust(3, '0')}_test"
                 for idx in list(range(30))]

    gen = lscat.generate_page(Path('.'))  # Path doesn't matter
    gen.send(None)

    # No need to shuffle, tracker already shuffles
    for pic in test_pics:
        gen.send(pic)

    # One for .thumbnail() and one for .show(), so total is 30+30
    thumb_calls = [x for x in mocked_pixcat.mock_calls if x[1]]
    thumb_calls_args = [x[1] for x in thumb_calls]
    assert len(mocked_pixcat.mock_calls) == 60
    assert len(thumb_calls) == 30
    # Default thumbnail size
    assert thumb_calls_args == [(310,)] * 30

    show_calls = [x for x in mocked_pixcat.mock_calls if not x[1]]
    kwargs = [x[2] for x in show_calls]
    align = [x['align'] for x in kwargs]
    xcoords = [x['x'] for x in kwargs]
    ycoords = [x['y'] for x in kwargs]
    assert align == ['left'] * 30
    assert xcoords == [2, 20, 38, 56, 74] * 6
    assert ycoords == [0, 0, 0, 0, 0, 9, 9, 9, 9, 9] * 3

def test_generate_users(monkeypatch):
    mocked_pixcat = Mock()
    monkeypatch.setattr("koneko.lscat.Image", lambda *a, **k: mocked_pixcat)
    monkeypatch.setattr("koneko.lscat.Terminal.width", 100)
    monkeypatch.setattr("koneko.lscat.Terminal.height", 20)

    test_pics = [f"{str(idx).rjust(3, '0')}_test"
                 for idx in list(range(120))]

    gen = lscat.generate_users(Path('.'))  # Path doesn't matter
    gen.send(None)

    # No need to shuffle, tracker already shuffles
    for pic in test_pics:
        gen.send(pic)

    # One for .thumbnail() and one for .show(), so total is 30+30
    thumb_calls = [x for x in mocked_pixcat.mock_calls if x[1]]
    thumb_calls_args = [x[1] for x in thumb_calls]
    assert len(mocked_pixcat.mock_calls) == 240
    assert len(thumb_calls) == 120
    # Default thumbnail size
    assert thumb_calls_args == [(310,)] * 120

    show_calls = [x for x in mocked_pixcat.mock_calls if not x[1]]
    kwargs = [x[2] for x in show_calls]
    align = [x['align'] for x in kwargs]
    xcoords = [x['x'] for x in kwargs]
    ycoords = [x['y'] for x in kwargs]
    assert align == ['left'] * 120
    assert xcoords == [2, 39, 57, 75] * 30
    assert ycoords == [0] * 120

