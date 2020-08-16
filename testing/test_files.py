import os
from pathlib import Path
from collections import namedtuple

import pytest

from koneko import files, KONEKODIR


def test_write_read_token_file(tmp_path):
    token_dir = tmp_path / 'token'
    files.write_token_file(token_dir, 'example_token')
    assert files.read_token_file(token_dir) == 'example_token'


def test_read_token_file_no_token(tmp_path):
    assert files.read_token_file(tmp_path) is None


def test_find_mode2_dirs(tmp_path, monkeypatch):
    monkeypatch.setattr('koneko.files.KONEKODIR', tmp_path)
    (tmp_path / '1234' / 'individual').mkdir(parents=True)
    (tmp_path / '5678' / '1').mkdir(parents=True)
    assert files.find_mode2_dirs() == ['1234']


def test_verify_full_download():
    assert files.verify_full_download('testing/files/008_77803142_p0.png') is True
    assert files.verify_full_download('testing/files/not_an_image.txt') is False
    # The above code will remove the file
    os.system('touch testing/files/not_an_image.txt')


@pytest.mark.parametrize('mkdir', (True, False))
def test_remove_dir_if_exist(tmp_path, mkdir):
    path = tmp_path / 'test'
    if mkdir:
        path.mkdir()
    FakeData = namedtuple('data', ('download_path',))
    data = FakeData(path)
    files.remove_dir_if_exist(data)
    assert not path.is_dir()


def test_filter_history(tmp_path):
    (tmp_path / 'history').touch()
    assert files.filter_history(tmp_path) == []


def data_faker(tmp_path):
    FakeData = namedtuple('data', ('download_path', 'first_img', 'all_names'))
    data = FakeData(
        tmp_path, '004_祝！！！.jpg',
        ['004_祝！！！.jpg', '008_77803142_p0.png', '017_ミコニャン.jpg']
    )
    return data

def test_dir_not_empty_dir_exists_but_empty(tmp_path):
    """Test dir exists but is empty"""
    data = data_faker(tmp_path)
    tmp_path.mkdir(exist_ok=True)
    assert files.dir_not_empty(data) is False

def test_dir_not_empty_incomplete_dir(tmp_path):
    data = data_faker(tmp_path)
    # Copy .koneko and only one image to that dir
    (tmp_path / '.koneko').touch()
    os.system(f'cp testing/files/004_祝！！！.jpg {tmp_path}')

    assert files.dir_not_empty(data) is False

def test_dir_not_empty_all_images_present(tmp_path):
    data = data_faker(tmp_path)
    # Copy all images to dir
    for f in data.all_names:
        os.system(f'cp testing/files/{f} {tmp_path}')

    assert files.dir_not_empty(data)


def test_filter_dir_1(monkeypatch, tmp_path):
    monkeypatch.setattr('koneko.files.KONEKODIR', tmp_path)
    (tmp_path / '1234').touch()
    (tmp_path / 'testgallery').touch()
    (tmp_path / 'notanumber').touch()

    assert set(files.filter_dir(['1'])) == {'testgallery', '1234'}


def test_filter_dir_2(monkeypatch, tmp_path):
    monkeypatch.setattr('koneko.files.KONEKODIR', tmp_path)
    (tmp_path / '1234').mkdir()
    (tmp_path / '1234' / 'individual').touch()
    (tmp_path / '5678').mkdir()
    (tmp_path / 'testgallery').touch()
    (tmp_path / 'notanumber').touch()

    assert files.filter_dir(['2']) == ['1234']


def test_filter_dir_3(monkeypatch, tmp_path):
    monkeypatch.setattr('koneko.files.KONEKODIR', tmp_path)
    (tmp_path / '1234').touch()
    (tmp_path / 'testuser').touch()
    (tmp_path / 'following').touch()

    assert set(files.filter_dir(['3'])) == {'following', 'testuser'}


def test_filter_dir_4(monkeypatch, tmp_path):
    monkeypatch.setattr('koneko.files.KONEKODIR', tmp_path)
    (tmp_path / '1234').touch()
    (tmp_path / 'search').touch()
    (tmp_path / 'notanumber').touch()

    assert files.filter_dir(['4']) == ['search']

def test_filter_dir_5(monkeypatch, tmp_path):
    monkeypatch.setattr('koneko.files.KONEKODIR', tmp_path)
    (tmp_path / '1234').touch()
    (tmp_path / 'illustfollow').touch()
    (tmp_path / 'notanumber').touch()

    assert files.filter_dir(['5']) == ['illustfollow']


def test_valid_mode1_valid():
    assert files.valid_mode1(KONEKODIR / '123/1') is True

def test_valid_mode2_valid():
    assert files.valid_mode2(KONEKODIR / '123/individual/123') is True

def test_valid_mode2_individual_with_file_is_valid(monkeypatch, tmp_path):
    # A bare 'individual' dir is valid if and only if there are files inside
    monkeypatch.setattr('koneko.KONEKODIR', tmp_path)
    mydir = tmp_path / '123/individual/'
    myfile = mydir / 'testfile'
    mydir.mkdir(parents=True)
    myfile.touch()
    assert files.valid_mode2(mydir) is True

def test_valid_mode2_individual_with_file_and_dir_is_valid(monkeypatch, tmp_path):
    # Differs from previous test by only one line: the one after the comment
    monkeypatch.setattr('koneko.KONEKODIR', tmp_path)
    mydir = tmp_path / '123/individual/'
    myfile = mydir / 'testfile'
    mydir.mkdir(parents=True)
    myfile.touch()
    # Dir contains file and dir, still valid
    (mydir / 'newdir').mkdir()
    assert files.valid_mode2(mydir) is True

def test_valid_mode3_valid():
    assert files.valid_mode3(KONEKODIR / 'following/123/1') is True

def test_valid_mode4_valid():
    assert files.valid_mode4(KONEKODIR / 'search/searchstr/1') is True

def test_valid_mode5_valid():
    assert files.valid_mode5(KONEKODIR / 'illustfollow/1') is True


root = '123'
mode1_paths = ('123/1',)
mode2_paths = ('123/individual', '123/individual/1')
mode3_paths = ('following', 'following/2232374', 'following/2232374/1')
mode4_paths = ('search', 'search/searchstr', 'search/searchstr/1')
mode5_paths = ('illustfollow', 'illustfollow/1')


@pytest.mark.parametrize('path', (
    root,
    *mode2_paths,
    *mode3_paths,
    *mode4_paths,
    *mode5_paths,
    )
)
def test_valid_mode1_invalid(path):
    assert files.valid_mode1(KONEKODIR / path) is False


@pytest.mark.parametrize('path', (
    root,
    *mode1_paths,
    *mode3_paths,
    *mode4_paths,
    *mode5_paths,
    )
)
def test_valid_mode2_invalid(path):
    assert files.valid_mode2(KONEKODIR / path) is False

def test_valid_mode2_individual_no_files_is_invalid(monkeypatch, tmp_path):
    # A bare 'individual' dir is valid if and only if it contains files only
    monkeypatch.setattr('koneko.KONEKODIR', tmp_path)
    mydir = tmp_path / '123/individual/'
    mydir.mkdir(parents=True)
    # No files in dir: invalid
    assert files.valid_mode2(mydir) is False

def test_valid_mode2_individual_dirs_only_is_invalid(monkeypatch, tmp_path):
    # A bare 'individual' dir is valid if and only if it contains files only
    monkeypatch.setattr('koneko.KONEKODIR', tmp_path)
    mydir = tmp_path / '123/individual/'
    mydir.mkdir(parents=True)
    # dir only has other dirs
    (mydir / 'newdir').mkdir()
    assert files.valid_mode2(mydir) is False


@pytest.mark.parametrize('path', (
    root,
    *mode1_paths,
    *mode2_paths,
    *mode4_paths,
    *mode5_paths,
    )
)
def test_valid_mode3_invalid(path):
    assert files.valid_mode3(KONEKODIR / path) is False


@pytest.mark.parametrize('path', (
    root,
    *mode1_paths,
    *mode2_paths,
    *mode3_paths,
    *mode5_paths,
    )
)
def test_valid_mode4_invalid(path):
    assert files.valid_mode4(KONEKODIR / path) is False

@pytest.mark.parametrize('path', (
    root,
    *mode1_paths,
    *mode2_paths,
    *mode3_paths,
    *mode4_paths,
    )
)
def test_valid_mode5_invalid(path):
    assert files.valid_mode5(KONEKODIR / path) is False


@pytest.mark.parametrize('path', (
    '123/1',
    '123/individual/1',
    'following/2232374/1',
    'search/searchstr/1',
    'illustfollow/1',
    )
)
def test_path_valid_full_are_valid(path):
    assert files.path_valid(KONEKODIR / path) is True


@pytest.mark.parametrize('path', (
    '123',
    'following',
    'following/2232374',
    'search',
    'search/searchstr',
    'illustfollow',
    )
)
def test_path_valid_partial_are_invalid(path):
    assert files.path_valid(KONEKODIR / path) is False
