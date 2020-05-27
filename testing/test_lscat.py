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
    assert showed == [
        'test_config.ini',
        '17_ミコニャン.jpg',
        'mode3.json',
        'mode1.json',
        '04_祝！！！.jpg',
        'not_an_image.txt',
        '77803142_p0.png',
        'mode2.json'
    ]

    captured = capsys.readouterr()
    assert captured.out == '         1                 2                 3                 4                 5 \n\n'
