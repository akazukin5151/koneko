import os
from pathlib import Path

import pytest

from koneko import files


def test_verify_full_download():
    assert files.verify_full_download("testing/files/008_77803142_p0.png") is True
    assert files.verify_full_download("testing/files/not_an_image.txt") is False
    # The above code will remove the file
    os.system("touch testing/files/not_an_image.txt")

def test_dir_not_empty():
    class FakeData:
        def __init__(self):
            self.download_path = Path('testing/files/empty_dir')
            self.first_img = "004_祝！！！.jpg"
            self.all_names = ["004_祝！！！.jpg", '008_77803142_p0.png', '017_ミコニャン.jpg']

    data = FakeData()

    try:
        # Test dir exists but is empty
        Path('testing/files/empty_dir').mkdir()
        data.download_path = Path('testing/files/empty_dir')
        assert files.dir_not_empty(data) is False

        # Copy .koneko and only one image to that dir
        os.system('touch testing/files/empty_dir/.koneko')
        os.system('cp testing/files/004_祝！！！.jpg testing/files/empty_dir/')

        assert files.dir_not_empty(data) is False

        # Copy all images to dir
        for f in ('008_77803142_p0.png', '017_ミコニャン.jpg'):
            os.system(f'cp testing/files/{f} testing/files/empty_dir/')

        assert files.dir_not_empty(data)

    finally:
        os.system('rm -r testing/files/empty_dir')

    # Test Throw some errors
    class FakeData:
        def __init__(self):
            self.download_path = Path('testing/files/')

        @property
        def first_img(self):
            raise KeyError

    data = FakeData()
    assert files.dir_not_empty(data)

    class FakeData:
        def __init__(self):
            self.download_path = Path('testing/files/')

        @property
        def first_img(self):
            raise AttributeError

    data = FakeData()
    assert files.dir_not_empty(data)
