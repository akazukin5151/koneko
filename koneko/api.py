"""Handles (almost) all Pixiv API interactions, eg async login, requests"""

import queue
import threading

import funcy
from pixivpy3 import PixivError, AppPixivAPI

from koneko import pure


class APIHandler:
    def __init__(self):
        self.api_queue = queue.Queue()
        self.api_thread = threading.Thread(target=self._login)
        self._credentials: 'Dict'
        self.api: 'AppPixivAPI()'

    def add_credentials(self, credentials):
        self._credentials = credentials

    def start(self):
        """Start logging in"""
        self.api_thread.start()

    def await_login(self):
        """Wait for login to finish, then assign PixivAPI session to API"""
        self.api_thread.join()
        self.api = self.api_queue.get()

    def _login(self):
        """
        Logins to pixiv in the background, using credentials from config file.
        """
        api = AppPixivAPI()
        api.login(self._credentials['Username'], self._credentials['Password'])
        self.api_queue.put(api)


    # API request functions for each mode
    @funcy.retry(tries=3, errors=(ConnectionError, PixivError))
    def parse_next(self, next_url):
        """All modes; parse next_url for next page's json"""
        return self.api.parse_qs(next_url)

    @funcy.retry(tries=3, errors=(ConnectionError, PixivError))
    @pure.spinner('')
    def artist_gallery_parse_next(self, **kwargs):
        """Mode 1, feed in next page"""
        return self.api.user_illusts(**kwargs)

    @funcy.retry(tries=3, errors=(ConnectionError, PixivError))
    @pure.spinner('')
    def artist_gallery_request(self, artist_user_id):
        """Mode 1, normal usage"""
        return self.api.user_illusts(artist_user_id)

    @funcy.retry(tries=3, errors=(ConnectionError, PixivError))
    def protected_illust_detail(self, image_id):
        """Mode 2"""
        return self.api.illust_detail(image_id)

    @funcy.retry(tries=3, errors=(ConnectionError, PixivError))
    def search_user_request(self, searchstr, offset):
        """Mode 3"""
        return self.api.search_user(searchstr, offset=offset)

    @funcy.retry(tries=3, errors=(ConnectionError, PixivError))
    def following_user_request(self, user_id, publicity, offset):
        """Mode 4"""
        return self.api.user_following(user_id, restrict=publicity, offset=offset)

    @funcy.retry(tries=3, errors=(ConnectionError, PixivError))
    @pure.spinner('')
    def illust_follow_request(self, **kwargs):
        """Mode 5
        **kwargs can be **parse_page (for _prefetch_next_page), but also contain
        publicity='private' (for normal)
        """
        return self.api.illust_follow(**kwargs)

    # Download
    @funcy.retry(tries=3, errors=(ConnectionError, PixivError))
    def protected_download(self, url):
        """Protect api download function with funcy.retry so it doesn't crash"""
        self.api.download(url)

myapi = APIHandler()
