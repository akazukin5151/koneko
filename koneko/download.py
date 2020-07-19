"""Download functions. See ../puml/download.puml. All of them download through
_download_with_tracker(), which downloads through api.myapi.protected_download().

The _async_filter_and_download() branch is for downloading multiple images, and includes:
    - init_download()
        - _async_download_rename()
    - async_download_no_rename()
        - async_download_spinner()

- ui Gallery and User calls init_download() to reach _async_download_rename()
- async_download_no_rename() can be directly called, or wrapped with a spinner by
  async_download_spinner()

The download_url() branch is for downloading one image/url (for mode 2), and includes:
    - download_url_verified(): a wrapper that ensures png images are properly downloaded
    - download_image_num(): for downloading an image given its number in gallery modes
        - download_image_coords(): wrapper given its coords not number

Only download_image_num() and download_image_coords() is reachable from prompt,
but ui can call download_url()
"""

import os
import itertools
from pathlib import Path
from shutil import rmtree
from collections import namedtuple
from concurrent.futures import ThreadPoolExecutor

from funcy import autocurry

from koneko.data import UserData
from koneko import api, pure, utils, files


# - Wrappers around download functions, for downloading multi-images
def save_number_of_artists(data) -> 'IO':
    """"Save the number of artists == splitpoint
    So later accesses, which will not request, can display properly
    """
    with open(data.download_path / '.koneko', 'w') as f:
        f.write(str(data.splitpoint))


def init_download(data: 'data.<class>', tracker: 'lscat.<class>') -> 'IO':
    """Download the illustrations of one page  and rename them."""
    if files.dir_not_empty(data):
        return True

    if data.page_num == 1:
        print('Cache is outdated, reloading...')
    if data.download_path.is_dir():
        rmtree(data.download_path)

    if data.all_urls:
        _async_download_rename(data, tracker)

    if isinstance(data, UserData):
        save_number_of_artists(data)


# - Download functions for multiple images
def _async_download_rename(data, tracker=None) -> 'IO':
    newnames = itertools.filterfalse(os.path.isfile, data.newnames_with_ext)
    _async_filter_and_download(data, newnames, tracker)


def async_download_no_rename(download_path, urls, tracker=None) -> 'IO':
    if not urls:
        return True

    FakeData = namedtuple('data', ('download_path', 'all_urls'))
    data = FakeData(download_path, urls)
    names = itertools.cycle((None,))

    _async_filter_and_download(data, names, tracker)


@utils.spinner('')
def async_download_spinner(download_path: Path, urls) -> 'IO':
    """Batch download in background with spinner. For mode 2; multi-image posts"""
    async_download_no_rename(download_path, urls)


# Might multiprocessing be faster to bypass urllib's pool limit?
# The lscat generator cannot be pickled, as well as api.myapi
# essentially everything has to be rewritten with multiprocessing in mind

def _async_filter_and_download(data, newnames, tracker):
    helper = _download_with_tracker(path=data.download_path, tracker=tracker)
    os.makedirs(data.download_path, exist_ok=True)
    with ThreadPoolExecutor(max_workers=len(data.all_urls)) as executor:
        executor.map(helper, data.all_urls, newnames)


@autocurry
def _download_with_tracker(url, img_name, path, tracker) -> 'IO':
    """Actually downloads one pic given one url"""
    api.myapi.protected_download(url, path, img_name)
    if tracker:
        tracker.update(img_name)


# - Wrappers around the core functions for downloading one image
# - Synchronous download functions, does not download in background
@utils.spinner('')
def download_url(download_path: Path, url, filename: str) -> 'IO':
    """Downloads one url, intended for single images only"""
    os.makedirs(download_path, exist_ok=True)
    if not Path(filename).is_file():
        print('   Downloading illustration...', flush=True, end='\r')
        api.myapi.protected_download(url, download_path, filename)


def download_url_verified(url, png=False) -> 'IO':
    # Returned url might be different if png is True
    url, filename, filepath = pure.full_img_details(url, png=png)
    download_path = Path('~/Downloads').expanduser()

    download_url(download_path, url, filename)

    verified = files.verify_full_download(filepath)
    if not verified:
        download_url_verified(url, png=True)
    else:
        print(f'Image downloaded at {filepath}')


# Download full res from ui, on user demand (from prompt)
def download_image_num(data, number) -> 'IO':
    download_url_verified(data.url(number))


def download_image_coords(data, first_num, second_num) -> 'IO':
    selected_image_num = utils.find_number_map(int(first_num), int(second_num))
    # 0 is acceptable, but is falsy; but 0 'is not' False
    if selected_image_num is False:
        print('Invalid number!')
    else:
        download_image_num(data, selected_image_num)

