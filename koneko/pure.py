"""
Collection of functions that are pure and side effect free
    (Excluding print)
"""

import os
import re
from pathlib import Path

import funcy
import cytoolz

from koneko import colors as c


def split_backslash_last(string: str) -> str:
    """
    Intended for splitting url to get filename, but it has lots of applications...
    """
    return string.split('/')[-1]


def generate_filepath(filename: str) -> Path:
    return Path('~').expanduser() / 'Downloads' / filename

@cytoolz.curry
def prefix_filename(old_name_with_ext: str, new_name: str, number: int) -> str:
    """old_name_with_ext can be `test.png`, but new_name is `abcd`"""
    img_ext = old_name_with_ext.split('.')[-1]
    number_prefix = str(number).rjust(3, '0')
    new_file_name = f'{number_prefix}_{new_name}.{img_ext}'
    return new_file_name

def prefix_artist_name(name: str, number: int) -> str:
    number_prefix = str(number).rjust(2, '0')
    new_file_name = f"{number_prefix}\n{' ' * 19}{name}"
    return new_file_name

def print_multiple_imgs(illusts_json: 'Json') -> None:
    _ = [print(f'{c.RED}#{index}{c.RESET} has {c.BLUE}{pages}{c.RESET} pages', end=', ')
         for (index, json) in enumerate(illusts_json)
         if (pages := json['page_count']) > 1]
    print('')


@cytoolz.curry
def url_given_size(post_json: 'Json', size: str) -> str:
    """
    size : str
        One of: ("square-medium", "medium", "large")
    """
    return post_json['image_urls'][size]


@cytoolz.curry
def post_title(current_page_illusts: 'Json', post_number: int) -> str:
    return current_page_illusts[post_number]['title']


def medium_urls(current_page_illusts: 'Json') -> 'list[str]':
    get_medium_url = url_given_size(size='square_medium')
    urls = list(map(get_medium_url, current_page_illusts))
    return urls


def post_titles_in_page(current_page_illusts: 'Json') -> 'list[str]':
    post_titles = post_title(current_page_illusts)
    titles = list(map(post_titles, range(len(current_page_illusts))))
    return titles


def page_urls_in_post(post_json: 'Json', size='medium') -> (int, 'list[str]'):
    """Get the number of pages and each of their urls in a multi-image post."""
    number_of_pages = post_json['page_count']
    if number_of_pages > 1:
        list_of_pages = post_json['meta_pages']
        page_urls = [url_given_size(list_of_pages[i], size)
                     for i in range(number_of_pages)]
    else:
        page_urls = [url_given_size(post_json, size)]

    return number_of_pages, page_urls


def change_url_to_full(url: str, png=False) -> str:
    """
    The 'large' resolution url isn't the largest. This uses changes the url to
    the highest resolution available
    """
    url = re.sub(r'_master\d+', '', url)
    url = re.sub(r'c\/\d+x\d+_\d+_\w+\/img-master', 'img-original', url)

    # If it doesn't work, try changing to png
    if png:
        url = url.replace('jpg', 'png')
    return url

@funcy.decorator
def catch_ctrl_c(call: 'func[T]') -> 'T':
    """
    See http://hackflow.com/blog/2013/11/03/painless-decorators/
    """
    try:
        return call()
    except KeyboardInterrupt:
        os.system('clear')

def process_user_url(url_or_id: str) -> str:
    if 'users' in url_or_id:
        if '\\' in url_or_id:
            user_input = split_backslash_last(url_or_id).split('\\')[-1][1:]
        else:
            user_input = split_backslash_last(url_or_id)
    else:
        user_input = url_or_id
    return user_input

def process_artwork_url(url_or_id: str) -> str:
    if 'artworks' in url_or_id:
        user_input = split_backslash_last(url_or_id).split('\\')[0]
    elif 'illust_id' in url_or_id:
        user_input = re.findall(r'&illust_id.*', url_or_id)[0].split('=')[-1]
    else:
        user_input = url_or_id
    return user_input
