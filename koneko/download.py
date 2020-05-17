import os
import itertools
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor

import cytoolz

from koneko import api, pure, utils


@pure.spinner('')
def async_download_spinner(download_path, urls, rename_images=False,
                           file_names=None, pbar=None):
    """Batch download and rename, with spinner. For mode 2; multi-image posts"""
    async_download_core(
        download_path,
        urls,
        rename_images=rename_images,
        file_names=file_names,
        pbar=pbar,
    )

def async_download_core(download_path, urls, rename_images=False,
                        file_names=None, pbar=None, tracker=None):
    """
    Rename files with given new name if needed.
    Submit each url to the ThreadPoolExecutor, so download and rename are concurrent
    """
    os.makedirs(download_path, exist_ok=True)

    oldnames = list(map(pure.split_backslash_last, urls))
    if rename_images:
        newnames = map(pure.prefix_filename, oldnames, file_names, range(len(urls)))
        newnames = list(newnames)
    else:
        newnames = oldnames

    filtered = itertools.filterfalse(os.path.isfile, newnames)
    oldnames = itertools.filterfalse(os.path.isfile, oldnames)
    helper = downloadr(pbar=pbar, tracker=tracker)

    with pure.cd(download_path):
        with ThreadPoolExecutor(max_workers=len(urls)) as executor:
            executor.map(helper, urls, oldnames, filtered)


@cytoolz.curry
def downloadr(url, img_name, new_file_name=None, pbar=None, tracker=None):
    """Actually downloads one pic given one url, rename if needed."""
    api.myapi.protected_download(url)

    if pbar:
        pbar.update(1)
    # print(f"{img_name} done!")
    if new_file_name:
        # This character break renames
        if '/' in new_file_name:
            new_file_name = new_file_name.replace('/', '')
        os.rename(img_name, new_file_name)
        img_name = new_file_name

    if tracker:
        tracker.update(img_name)


def download_page(data, pbar=None, tracker=None):
    """
    Download the illustrations on one page of given artist id (using threads),
    rename them based on the *post title*. Used for gallery modes (1 and 5)
    """
    urls = pure.medium_urls(data.current_illusts)
    titles = pure.post_titles_in_page(data.current_illusts)

    async_download_core(
        data.download_path, urls, rename_images=True, file_names=titles, pbar=pbar,
        tracker=tracker
    )

# - Wrappers around above download functions, for downloading multi-images
def user_download(data, tracker=None):
    preview_path = data.download_path / 'previews'
    async_download_core(
        preview_path,
        data.all_urls,
        rename_images=True,
        file_names=data.all_names,
        pbar=None,
        tracker=tracker
    )

    # Move artist profile pics to their correct dir
    to_move = sorted(os.listdir(preview_path))[:data.splitpoint]
    with pure.cd(data.download_path):
        _ = [os.rename(data.download_path / 'previews' / pic, data.download_path / pic)
         for pic in to_move]

def init_download(data, download_func, tracker):
    if (data.download_path.is_dir() and
        data.first_img not in sorted(os.listdir(data.download_path))[0]):
        # Remove old images
        if data.page_num == 1:
            print('Cache is outdated, reloading...')
        os.system(f'rm -r {data.download_path}') # shutil.rmtree is better

    download_func(data, tracker=tracker)

# - Wrappers around the core functions for downloading one image
@pure.spinner('')
def download_core(large_dir, url, filename, try_make_dir=True):
    """Downloads one url, intended for single images only"""
    if try_make_dir:
        os.makedirs(large_dir, exist_ok=True)
    if not Path(filename).is_file():
        print('   Downloading illustration...', flush=True, end='\r')
        with pure.cd(large_dir):
            downloadr(url, filename, None)


def download_image_verified(image_id=None, post_json=None, png=False, **kwargs):
    """
    This downloads an image, checks if it's valid. If not, retry with png.
    Used for downloading full-res, single only; on-user-demand
    """
    if png and 'url' in kwargs: # Called from recursion
        # IMPROVEMENT This is copied from full_img_details()...
        url = pure.change_url_to_full(url=kwargs['url'], png=True)
        filename = pure.split_backslash_last(url)
        filepath = pure.generate_filepath(filename)

    elif not kwargs:
        url, filename, filepath = full_img_details(
            image_id=image_id, post_json=post_json, png=png
        )
    else:
        url = kwargs['url']
        filename = kwargs['filename']
        filepath = kwargs['filepath']

    download_path = Path('~/Downloads').expanduser()
    download_core(download_path, url, filename, try_make_dir=False)

    verified = utils.verify_full_download(filepath)
    if not verified:
        download_image_verified(url=url, png=True)
    else:
        print(f'Image downloaded at {filepath}\n')

@pure.spinner('Getting full image details... ')
def full_img_details(png=False, post_json=None, image_id=None):
    """
    All in one function that gets the full-resolution url, filename, and
    filepath of given image id. Or it can get the id given the post json
    """
    if image_id and not post_json:
        current_image = api.myapi.protected_illust_detail(image_id)

        post_json = current_image.illust

    url = pure.change_url_to_full(post_json=post_json, png=png)
    filename = pure.split_backslash_last(url)
    filepath = pure.generate_filepath(filename)
    return url, filename, filepath

