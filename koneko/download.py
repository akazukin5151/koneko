import os
import itertools
from pathlib import Path
from functools import partial
from concurrent.futures import ThreadPoolExecutor

from pipey import Pipeable as P

from koneko import api, pure, utils
from koneko.data import UserJson


# - Wrappers around download functions, for downloading multi-images
def save_number_of_artists(data) -> 'IO':
    """"Save the number of artists == splitpoint
    So later accesses, which will not request, can display properly
    """
    with utils.cd(data.download_path):
        with open('.koneko', 'w') as f:
            f.write(str(data.splitpoint))

def init_download(data, tracker) -> 'IO':
    """Download the illustrations of one page  and rename them."""
    if utils.dir_not_empty(data):
        return True

    if data.page_num == 1:
        print('Cache is outdated, reloading...')
    if data.download_path.is_dir():
        os.system(f'rm -r {data.download_path}')  # shutil.rmtree is better

    async_download_rename(data.download_path, data.all_urls, data.all_names, tracker)

    if isinstance(data, UserJson):
        save_number_of_artists(data)


# - Download functions for multiple images
def newnames_with_ext(urls, oldnames_with_ext, newnames: 'list[str]') -> 'list[str]':
    return (
        urls
        >> P(len)
        >> P(range)
        >> P(lambda r: map(pure.prefix_filename, oldnames_with_ext, newnames, r))
        >> P(list)
    )

# private
def async_download_rename(download_path, urls, newnames, tracker=None) -> 'IO':
    oldnames_ext = urls >> pure.Map(pure.split_backslash_last)
    newnames_ext = newnames_with_ext(urls, oldnames_ext, newnames)
    async_filter_and_download(download_path, urls, oldnames_ext, newnames_ext, tracker)

def async_download_no_rename(download_path, urls, tracker=None) -> 'IO':
    oldnames_ext = urls >> pure.Map(pure.split_backslash_last)
    async_filter_and_download(download_path, urls, oldnames_ext, oldnames_ext, tracker)

@utils.spinner('')
def async_download_spinner(download_path: Path, urls) -> 'IO':
    """Batch download in background with spinner. For mode 2; multi-image posts"""
    async_download_no_rename(download_path, urls)


# private
def async_filter_and_download(download_path, urls, oldnames_with_ext, newnames_with_ext,
                              tracker=None) -> 'IO':
    """
    Submit each url to the ThreadPoolExecutor to download and rename in background
    """
    # Nothing needs to be downloaded
    if not urls:
        return True

    # Filter out already downloaded files
    downloaded_newnames = itertools.filterfalse(os.path.isfile, newnames_with_ext)
    downloaded_oldnames = itertools.filterfalse(os.path.isfile, oldnames_with_ext)
    helper = partial(download_then_rename, tracker=tracker)

    os.makedirs(download_path, exist_ok=True)
    with utils.cd(download_path):
        with ThreadPoolExecutor(max_workers=len(urls)) as executor:
            executor.map(helper, urls, downloaded_oldnames, downloaded_newnames)


# private
def download_then_rename(url, img_name, new_file_name=None, tracker=None) -> 'IO':
    """Actually downloads one pic given one url, rename if needed."""
    api.myapi.protected_download(url)

    # Best to rename every completed download in their own thread
    # to avoid race conditions
    if new_file_name:
        # This character break renames
        if '/' in new_file_name:
            new_file_name = new_file_name.replace('/', '')
        os.rename(img_name, new_file_name)
        img_name = new_file_name

    if tracker:
        tracker.update(img_name)


# - Wrappers around the core functions for downloading one image
# - Synchronous download functions, does not download in background
@utils.spinner('')
def download_url(download_path: Path, url, filename: str, try_make_dir=True) -> 'IO':
    """Downloads one url, intended for single images only"""
    if try_make_dir:
        os.makedirs(download_path, exist_ok=True)
    if not Path(filename).is_file():
        print('   Downloading illustration...', flush=True, end='\r')
        with utils.cd(download_path):
            download_then_rename(url, filename)


def full_img_details(url: str, png=False) -> (str, str, Path):
    # Example of an image that needs to be downloaded in png: 77803142
    url = pure.change_url_to_full(url, png=png)
    filename = pure.split_backslash_last(url)
    filepath = pure.generate_filepath(filename)
    return url, filename, filepath

def download_url_verified(url, png=False) -> 'IO':
    # Returned url might be different if png is True
    url, filename, filepath = full_img_details(url, png=png)
    download_path = Path('~/Downloads').expanduser()

    download_url(download_path, url, filename, try_make_dir=False)

    verified = utils.verify_full_download(filepath)
    if not verified:
        download_url_verified(url, png=True)
    else:
        print(f'Image downloaded at {filepath}')

# Download full res from ui, on user demand
def download_image_coords(data, first_num, second_num) -> 'IO':
    selected_image_num = utils.find_number_map(int(first_num), int(second_num))
    # 0 is acceptable, but is falsy; but 0 'is not' False
    if selected_image_num is False:
        print('Invalid number!')
    else:
        download_image_num(data, selected_image_num)

def download_image_num(data, number) -> 'IO':
    # Update current_page_illusts, in case if you're in another page
    download_url_verified(data.url(number))
