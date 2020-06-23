import os
import sys
import random
from pathlib import Path
from unittest.mock import Mock

import pytest

from koneko import lscat, pure

# Lmao python
sys.path.append('testing')


def test_icat():
    try:
        lscat.icat("./testing/files/004_祝！！！.jpg")
    except OSError:
        # Github doesn't connect to terminal
        pass

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

    # This config has print_info = True
    # Can't use shared fixture because pathlib is used in FakeData
    monkeypatch.setattr('koneko.utils.Path.expanduser',
                        lambda x: Path('testing/test_config.ini'))

    fakedata = FakeData()
    lscat.show_instant(FakeTracker, fakedata, True)
    # First one works for me, but second one works for github
    assert set(showed) == {
        'test_config.ini', '004_祝！！！.jpg', 'mode3.json',
        'mode1.json', 'not_an_image.txt', '008_77803142_p0.png',
        '017_ミコニャン.jpg', 'mode2.json'
    } or set(showed) == {
        '004_祝！！！.jpg', 'mode3.json',
        'mode1.json', 'not_an_image.txt', '008_77803142_p0.png',
        '017_ミコニャン.jpg', 'mode2.json'
    }


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

    correct_order = pure.generate_orders(120, 30)
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
    monkeypatch.setattr("koneko.config.Terminal.width", 100)
    monkeypatch.setattr("koneko.config.Terminal.height", 20)

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
    monkeypatch.setattr("koneko.config.Terminal.width", 100)
    monkeypatch.setattr("koneko.config.Terminal.height", 20)

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

