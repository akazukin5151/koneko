"""
Collection of functions that are pure and side effect free
    (Excluding print)
"""

import os
import re
import itertools
import threading
from pathlib import Path
from contextlib import contextmanager

import funcy
import cytoolz
from colorama import Fore


@contextmanager
def cd(newdir):
    """
    Change current script directory, do something, change back to old directory
    See https://stackoverflow.com/questions/431684/how-do-i-change-the-working-directory-in-python/24176022#24176022

    Parameters
    ----------
    newdir : str
        New directory to cd into inside 'with'
    """
    prevdir = os.getcwd()
    os.chdir(os.path.expanduser(newdir))
    try:
        yield
    finally:
        os.chdir(prevdir)


def spin(done, message):
    for char in itertools.cycle('|/-\\'):  # Infinite loop
        print(message, char, flush=True, end='\r')
        if done.wait(0.1):
            break
    print(' ' * len(char), end='\r')  # clears the spinner


@funcy.decorator
def spinner(call, message=''):
    """
    See http://hackflow.com/blog/2013/11/03/painless-decorators/
    """
    done = threading.Event()
    spinner_thread = threading.Thread(target=spin, args=(done, message))
    spinner_thread.start()
    result = call()
    done.set()
    spinner_thread.join()
    return result


def split_backslash_last(string):
    """
    Intended for splitting url to get filename, but it has lots of applications...
    """
    return string.split('/')[-1]


def generate_filepath(filename):
    return Path('~').expanduser() / 'Downloads' / filename

@cytoolz.curry
def prefix_filename(old_name, new_name, number):
    img_ext = old_name.split('.')[-1]
    number_prefix = str(number).rjust(3, '0')
    new_file_name = f'{number_prefix}_{new_name}.{img_ext}'
    return new_file_name

def prefix_artist_name(name, number):
    number_prefix = str(number).rjust(2, '0')
    new_file_name = f"{number_prefix}\n{' ' * 19}{name}"
    return new_file_name

def find_number_map(x, y):
    if not (x >= 1 and y >= 1):
        return False
    # 5 = number of cols; 6 = number of rows, 30 images
    number_map = list(cytoolz.partition_all(5, range(30)))

    try:
        # coordinates are 1-based index
        number = number_map[y - 1][x - 1]
    except IndexError:
        print('Invalid number!\n')
        return False
    return number


def print_multiple_imgs(illusts_json):
    _red = Fore.RED
    _r = Fore.RESET
    _blue = Fore.BLUE
    [print(f'{_red}#{index}{_r} has {_blue}{pages}{_r} pages', end=', ')
     for (index, json) in enumerate(illusts_json)
     if (pages := json['page_count']) > 1]
    print('')


@cytoolz.curry
def url_given_size(post_json, size):
    """
    size : str
        One of: ("square-medium", "medium", "large")
    """
    return post_json['image_urls'][size]


@cytoolz.curry
def post_title(current_page_illusts, post_number):
    return current_page_illusts[post_number]['title']


def medium_urls(current_page_illusts):
    get_medium_url = url_given_size(size='square_medium')
    urls = list(map(get_medium_url, current_page_illusts))
    return urls


def post_titles_in_page(current_page_illusts):
    post_titles = post_title(current_page_illusts)
    titles = list(map(post_titles, range(len(current_page_illusts))))
    return titles


@spinner('')
def page_urls_in_post(post_json, size='medium'):
    """Get the number of pages and each of their urls in a multi-image post."""
    number_of_pages = post_json['page_count']
    if number_of_pages > 1:
        print(f'Page 1/{number_of_pages}')
        list_of_pages = post_json['meta_pages']
        page_urls = [url_given_size(list_of_pages[i], size)
                     for i in range(number_of_pages)]
    else:
        page_urls = [url_given_size(post_json, size)]

    return number_of_pages, page_urls


def change_url_to_full(post_json=None, png=False, url=None):
    """
    The 'large' resolution url isn't the largest. This uses changes the url to
    the highest resolution available
    """
    if post_json:
        url = url_given_size(post_json, 'large')
    url = re.sub(r'_master\d+', '', url)
    url = re.sub(r'c\/\d+x\d+_\d+_\w+\/img-master', 'img-original', url)

    # If it doesn't work, try changing to png
    if png:
        url = url.replace('jpg', 'png')
    return url

@funcy.decorator
def catch_ctrl_c(call):
    """
    See http://hackflow.com/blog/2013/11/03/painless-decorators/
    """
    try:
        result = call()
    except KeyboardInterrupt:
        os.system('clear')
    else:
        return result

def process_user_url(url_or_id):
    if 'users' in url_or_id:
        if '\\' in url_or_id:
            user_input = split_backslash_last(url_or_id).split('\\')[-1][1:]
        else:
            user_input = split_backslash_last(url_or_id)
    else:
        user_input = url_or_id
    return user_input, '1'

def process_artwork_url(url_or_id):
    if 'artworks' in url_or_id:
        user_input = split_backslash_last(url_or_id).split('\\')[0]
    elif 'illust_id' in url_or_id:
        user_input = re.findall(r'&illust_id.*', url_or_id)[0].split('=')[-1]
    return user_input, '2'
