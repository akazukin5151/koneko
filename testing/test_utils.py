import os
import sys
from pathlib import Path

import pytest

from koneko import utils

# Lmao python
sys.path.append('testing')


def test_find_number_map(monkeypatch):
    monkeypatch.setattr('koneko.utils.ncols_config', lambda: 5)
    assert ([utils.find_number_map(x, y)
             for y in range(1, 7)
             for x in range(1, 6)] == list(range(30)))
    assert not utils.find_number_map(0, 100)

    monkeypatch.setattr('koneko.utils.ncols_config', lambda: 6)
    assert [utils.find_number_map(x, y)
            for y in range(1, 7)
            for x in range(1, 7)][:30] == list(range(30))

def test_cd():
    current_dir = os.getcwd()
    with utils.cd(current_dir):
        testdir = os.getcwd()

    assert testdir == os.getcwd()
    assert os.getcwd() == current_dir


def test_verify_full_download():
    assert utils.verify_full_download("testing/files/008_77803142_p0.png") is True
    assert utils.verify_full_download("testing/files/not_an_image.txt") is False
    # The above code will remove the file
    os.system("touch testing/files/not_an_image.txt")

def test_dir_not_empty():
    class FakeData:
        def __init__(self):
            self.download_path = Path('testing/files/empty_dir')
            self.first_img = "004_祝！！！.jpg"
            self.all_names = ["004_祝！！！.jpg", '008_77803142_p0.png', '017_ミコニャン.jpg']

    data = FakeData()

    # Test dir exists but is empty
    # If mkdir throws because file exists, just delete it and re-run
    # In normal situations this dir will be deleted in line 69
    # However, if any code in between fails, it will not be deleted
    Path('testing/files/empty_dir').mkdir()
    data.download_path = Path('testing/files/empty_dir')
    assert utils.dir_not_empty(data) is False

    # Copy .koneko and only one image to that dir
    os.system('touch testing/files/empty_dir/.koneko')
    os.system('cp testing/files/004_祝！！！.jpg testing/files/empty_dir/')

    assert utils.dir_not_empty(data) is False

    # Copy all images to dir
    for f in ('008_77803142_p0.png', '017_ミコニャン.jpg'):
        os.system(f'cp testing/files/{f} testing/files/empty_dir/')

    assert utils.dir_not_empty(data)

    os.system('rm -r testing/files/empty_dir')

    # Test Throw some errors
    class FakeData:
        def __init__(self):
            self.download_path = Path('testing/files/')

        @property
        def first_img(self):
            raise KeyError

    data = FakeData()
    assert utils.dir_not_empty(data)

    class FakeData:
        def __init__(self):
            self.download_path = Path('testing/files/')

        @property
        def first_img(self):
            raise AttributeError

    data = FakeData()
    assert utils.dir_not_empty(data)


def test_frequent_history_items():
    counter = utils.frequent_history_mode('1')
    utils.format_frequent(counter)


