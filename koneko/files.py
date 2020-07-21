import os
import imghdr
from pathlib import Path
from shutil import rmtree

from placeholder import _
from funcy import curry, lfilter
from returns.pipeline import flow

from koneko import KONEKODIR


# Outbound IO
def remove_dir_if_exist(data) -> 'Maybe[IO]':
    if data.download_path.is_dir():
        rmtree(data.download_path)

def verify_full_download(filepath: Path) -> 'IO[bool]':
    verified = imghdr.what(filepath)
    if not verified:
        os.remove(filepath)
        return False
    return True


def write_token_file(token_dir, token) -> 'IO':
    with open(token_dir, 'w') as f:
        f.write(token)


# Inbound IO
def read_token_file(token_dir) -> 'Optional[str]':
    if token_dir.is_file():
        with open(token_dir, 'r') as f:
            return f.read()
    return None


def read_invis(data) -> 'IO[int]':
    with open(data.download_path / '.koneko', 'r') as f:
        return int(f.read())

def filter_history(path: 'Path') -> 'list[str]':
    return flow(
        path,
        os.listdir,
        sorted,
        curry(lfilter)(_ != 'history')
    )


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
    allowed_names = filter_modes_allowed(modes)
    predicate = filter_modes_predicate(modes, allowed_names)
    return [d for d in os.listdir(KONEKODIR) if predicate(d)]


def filter_modes_allowed(modes: 'list[str]') -> 'set[str]':
    """Pure"""
    allowed_names = set()
    if '1' in modes:
        allowed_names.add('testgallery')
    if '3' in modes:
        allowed_names.update(('following', 'testuser'))
    if '4' in modes:
        allowed_names.add('search')
    if '5' in modes:
        allowed_names.add('illustfollow')
    return allowed_names


def filter_modes_predicate(modes: 'list[str]', allowed_names: 'set[str]') -> 'func(str) -> bool':
    """Pure"""
    if '1' in modes:
        return lambda d: d.isdigit() or d in allowed_names
    elif '2' in modes:
        return lambda d: d in find_mode2_dirs() or d in allowed_names
    return lambda d: d in allowed_names


def find_mode2_dirs() -> 'list[str]':
    return [f for f in os.listdir(KONEKODIR)
            if f.isdigit()
            and 'individual' in os.listdir(KONEKODIR / f)]


def valid_mode1(path: 'Path') -> bool:
    return (
        path.parent.parent == KONEKODIR
        and path.name.isdigit()
        and 'following' not in str(path)
        and 'search' not in str(path)
        and 'illustfollow' not in str(path)
    )


def valid_mode2(path: 'Path') -> bool:
    # If the 'individual' dir only contains files, it is valid
    return (
        path.parent.name == 'individual'
        or (
            path.name == 'individual'
            and any([x.is_file() for x in os.scandir(path)])
        )
    )


def valid_mode3(path: 'Path') -> bool:
    return path.parent.parent.name == 'following'


def valid_mode4(path: 'Path') -> bool:
    return path.parent.parent.name == 'search'


def valid_mode5(path: 'Path') -> bool:
    return path.parent.name == 'illustfollow'


def path_valid(path: 'Path') -> bool:
    return any((
        valid_mode1(path),
        valid_mode2(path),
        valid_mode3(path),
        valid_mode4(path),
        valid_mode5(path),
    ))
