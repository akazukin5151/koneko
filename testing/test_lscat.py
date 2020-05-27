import os
import sys
from pathlib import Path

import pytest

# Lmao python
sys.path.append('../koneko/koneko')
sys.path.append('testing')

from koneko import lscat

def test_xcoords():
    assert lscat.xcoords(100) == [2, 20, 38, 56, 74]

def test_ycoords():
    assert lscat.ycoords(20) == [0, 9]

def test_icat():
    assert lscat.icat("testing/files/04_祝！！！.jpg") == None

def test_show_instant(capsys):
    showed = []

    class FakeTracker:
        def __init__(self, data):
            pass

        def update(self, new):
            showed.append(new)

    class FakeData:
        def __init__(self):
            self.download_path = Path('testing/files/')

    fakedata = FakeData()
    lscat.show_instant(FakeTracker, fakedata, True)
    assert set(showed) == set([
        'test_config.ini', '004_祝！！！.jpg', 'mode3.json',
        'mode1.json', 'not_an_image.txt', '008_77803142_p0.png',
        '017_ミコニャン.jpg', 'mode2.json'
    ])
    captured = capsys.readouterr()
    assert captured.out == '         1                 2                 3                 4                 5 \n\n'


def test_generate_orders():
    assert lscat.generate_orders(120, 30) == [0, 30, 31, 32, 1, 33, 34, 35, 2, 36, 37, 38, 3, 39, 40, 41, 4, 42, 43, 44, 5, 45, 46, 47, 6, 48, 49, 50, 7, 51, 52, 53, 8, 54, 55, 56, 9, 57, 58, 59, 10, 60, 61, 62, 11, 63, 64, 65, 12, 66, 67, 68, 13, 69, 70, 71, 14, 72, 73, 74, 15, 75, 76, 77, 16, 78, 79, 80, 17, 81, 82, 83, 18, 84, 85, 86, 19, 87, 88, 89, 20, 90, 91, 92, 21, 93, 94, 95, 22, 96, 97, 98, 23, 99, 100, 101, 24, 102, 103, 104, 25, 105, 106, 107, 26, 108, 109, 110, 27, 111, 112, 113, 28, 114, 115, 116, 29, 117, 118, 119]


def test_TrackDownloads():
    """Just running it to make sure it doesn't crash"""
    class FakeData:
        def __init__(self):
            self.download_path = Path('testing/files/gallery')

    data = FakeData()
    data.download_path.mkdir()
    for pic in ('004_祝！！！.jpg', '017_ミコニャン.jpg', '008_77803142_p0.png'):
        os.system(f'cp testing/files/{pic} testing/files/gallery/')

    lscat.show_instant(lscat.TrackDownloads, data)

    os.system(f'rm -r {data.download_path}')

def test_TrackDownloadsUser():
    """Just running it to make sure it doesn't crash"""
    class FakeData:
        def __init__(self):
            self.download_path = Path('testing/files/user')

    data = FakeData()
    data.download_path.mkdir()
    for pic in ('004_祝！！！.jpg', '017_ミコニャン.jpg', '008_77803142_p0.png'):
        os.system(f'cp testing/files/{pic} testing/files/user/')

    os.system('touch testing/files/user/.koneko')
    with open('testing/files/user/.koneko', 'w') as f:
        f.write("3")

    lscat.show_instant(lscat.TrackDownloadsUsers, data)

    os.system(f'rm -r {data.download_path}')
