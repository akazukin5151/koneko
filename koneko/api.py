"""Handles all Pixiv API interactions, eg async login, requests"""

import queue
import threading

import funcy
from pixivpy3 import PixivError, AppPixivAPI

from koneko import utils


class APIHandler:
    """Singleton that handles all the API interactions in the program"""
    def __init__(self):
        self._api_queue = queue.Queue()
        self._api_thread = threading.Thread(target=self._login)
        # Set in self.start() (because singleton is instantiated before config)
        self._credentials: 'dict[str, str]'
        # Set in self._await_login()
        self._api: 'AppPixivAPI'  # Object to login and request on

    @funcy.once
    def start(self, credentials):
        """Start logging in. The only setup entry point that is public"""
        self._credentials = credentials
        self._api_thread.start()

    @funcy.once
    def _await_login(self):
        """Wait for login to finish, then assign PixivAPI session to API"""
        self._api_thread.join()
        self._api = self._api_queue.get()

    def _login(self):
        """Logins to pixiv in the background, using credentials from config file"""
        api = AppPixivAPI()
        try:
            api.login(self._credentials['Username'], self._credentials['Password'])
        except PixivError as e:
            print('Login failed! Please correct your credentials in '
                  '~/.config/koneko/config.ini')
            print(e)
            print("Press 'q' and enter to exit")
            return

        self._api_queue.put(api)

    # Public API request functions for each mode
    @funcy.retry(tries=3, errors=(ConnectionError, PixivError))
    @utils.spinner('')
    def artist_gallery(self, artist_user_id, offset):
        """Mode 1"""
        self._await_login()
        return self._api.user_illusts(artist_user_id, offset=offset)

    @funcy.retry(tries=3, errors=(ConnectionError, PixivError))
    def protected_illust_detail(self, image_id):
        """Mode 2"""
        self._await_login()
        return self._api.illust_detail(image_id)

    @funcy.retry(tries=3, errors=(ConnectionError, PixivError))
    def following_user_request(self, user_id, publicity, offset):
        """Mode 3"""
        self._await_login()
        return self._api.user_following(user_id, restrict=publicity, offset=offset)

    @funcy.retry(tries=3, errors=(ConnectionError, PixivError))
    def search_user_request(self, searchstr, offset):
        """Mode 4"""
        self._await_login()
        return self._api.search_user(searchstr, offset=offset)

    @funcy.retry(tries=3, errors=(ConnectionError, PixivError))
    @utils.spinner('')
    def illust_follow_request(self, restrict, offset):
        """Mode 5"""
        self._await_login()
        return self._api.illust_follow(restrict=restrict, offset=offset)

    @funcy.retry(tries=3, errors=(ConnectionError, PixivError))
    @utils.spinner('')
    def illust_related_request(self, image_id, offset):
        """Mode 6"""
        self._await_login()
        return self._api.illust_related(illust_id=image_id, offset=offset)

    # Download
    @funcy.retry(tries=3, errors=(ConnectionError, PixivError))
    def protected_download(self, url, path, name):
        """Protect api download function with funcy.retry so it doesn't crash"""
        self._await_login()
        self._api.download(url, path=path, name=name)


myapi = APIHandler()
