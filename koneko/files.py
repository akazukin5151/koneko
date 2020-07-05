import os
import imghdr
from pathlib import Path
from shutil import rmtree

from placeholder import _
from funcy import curry, lfilter
from returns.pipeline import flow

from koneko import utils, KONEKODIR


def find_mode2_dirs():
    return [f for f in os.listdir(KONEKODIR)
            if f.isdigit()
            and 'individual' in os.listdir(KONEKODIR / f)]


def read_invis(data) -> 'IO[int]':
    with utils.cd(data.download_path):
        with open('.koneko', 'r') as f:
            return int(f.read())


def remove_dir_if_exist(data) -> 'Maybe[IO]':
    if data.download_path.is_dir():
        rmtree(data.download_path)


def filter_history(path):
    return flow(
        path,
        os.listdir,
        sorted,
        curry(lfilter)(_ != 'history')
    )


def verify_full_download(filepath: Path) -> 'IO[bool]':
    verified = imghdr.what(filepath)
    if not verified:
        os.remove(filepath)
        return False
    return True


def _dir_up_to_date(data, _dir) -> bool:
    # O(1) time
    if len(_dir) < len(data.all_names):
        return False

    # Should not fail because try-except early returned
    for name, _file in zip(data.all_names, sorted(_dir)):
        if name.replace('/', '') not in _file:
            return False
    return True


def dir_not_empty(data: 'Data') -> bool:
    if data.download_path.is_dir() and (_dir := os.listdir(data.download_path)):

        # Is a valid directory and it's not empty, but data has not been fetched yet
        try:
            data.all_names
        except (KeyError, AttributeError):
            return True

        # Exclude the .koneko file
        if '.koneko' in sorted(_dir)[0]:
            return _dir_up_to_date(data, sorted(_dir)[1:])

        return _dir_up_to_date(data, _dir)

    return False


def filter_dir(modes: 'list[str]') -> 'list[str]':
    """Given a list of modes to include, filter KONEKODIR to those modes"""
    path = KONEKODIR
    dirs = os.listdir(path)
    allowed_names = set()

    if '1' in modes:
        allowed_names.add('testgallery')
        predicate = lambda d: d.isdigit() or d in allowed_names

    if '2' in modes:
        mode2_dirs = find_mode2_dirs()
        predicate = lambda d: d in mode2_dirs or d in allowed_names

    if '3' in modes:
        allowed_names.update(('following', 'testuser'))

    if '4' in modes:
        allowed_names.add('search')

    if '5' in modes:
        allowed_names.add('illustfollow')

    if '1' not in modes and '2' not in modes:
        predicate = lambda d: d in allowed_names

    return [d for d in dirs if predicate(d)]
