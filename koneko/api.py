"""Handles all Pixiv API interactions, eg async login, requests"""

import threading

import funcy
from pixivpy3 import PixivError, AppPixivAPI

from koneko import utils


class APIHandler:
    """Singleton that handles all the API interactions in the program"""

    def __init__(self):
        self._api_thread = threading.Thread(target=self._login)
        self._login_started = False
        self._login_done = False

        self._api = AppPixivAPI()  # Object to login and request on
        # Set in self.start() (because singleton is instantiated before config)
        self._credentials: 'dict[str, str]'
        self._response: 'Json'

    def start(self, credentials):
        """Start logging in. The only setup entry point that is public"""
        if not self._login_started:
            self._credentials = credentials
            self._api_thread.start()
            self._login_started = True

    def _await_login(self):
        """Wait for login to finish, then assign PixivAPI session to API"""
        if not self._login_done:
            self._api_thread.join()
            self._login_done = True

    def _login(self):
        self._login_with_token()

    def _login_with_token(self):
        # TODO: refresh the token if it expired
        try:
            self._response = self._api.auth(
                refresh_token=self._credentials['refresh_token']
            )
        except PixivError as e:
            print('')
            print(e)
            print('If this is a cloudflare captcha issue, just quit and retry')
            print('It is not a problem with koneko or pixivpy')
            print(
                'Otherwise, please report to '
                'https://github.com/akazukin5151/koneko/issues'
            )
            print("Press 'q' and enter to exit")
        #else:
            #print('Login success!')

    # Public API requests for user id
    def get_user_id(self) -> 'Json':
        self._await_login()
        return self._response['user']['id']

    # Public API request functions for each mode
    @funcy.retry(tries=3, errors=(ConnectionError, PixivError))
    @utils.spinner('')
    def artist_gallery(self, artist_user_id, offset) -> 'Json':
        """Mode 1"""
        self._await_login()
        return self._api.user_illusts(artist_user_id, offset=offset)

    @funcy.retry(tries=3, errors=(ConnectionError, PixivError))
    def protected_illust_detail(self, image_id) -> 'Json':
        """Mode 2"""
        self._await_login()
        return self._api.illust_detail(image_id)

    @funcy.retry(tries=3, errors=(ConnectionError, PixivError))
    def following_user_request(self, user_id, publicity, offset) -> 'Json':
        """Mode 3"""
        self._await_login()
        return self._api.user_following(user_id, restrict=publicity, offset=offset)

    @funcy.retry(tries=3, errors=(ConnectionError, PixivError))
    def search_user_request(self, searchstr, offset) -> 'Json':
        """Mode 4"""
        self._await_login()
        return self._api.search_user(searchstr, offset=offset)

    @funcy.retry(tries=3, errors=(ConnectionError, PixivError))
    @utils.spinner('')
    def illust_follow_request(self, restrict, offset) -> 'Json':
        """Mode 5"""
        self._await_login()
        return self._api.illust_follow(restrict=restrict, offset=offset)

    @funcy.retry(tries=3, errors=(ConnectionError, PixivError))
    @utils.spinner('')
    def illust_related_request(self, image_id, offset) -> 'Json':
        """Mode 15 (1.5 * 10 so it's an int)"""
        self._await_login()
        return self._api.illust_related(illust_id=image_id, offset=offset)

    @funcy.retry(tries=3, errors=(ConnectionError, PixivError))
    @utils.spinner('')
    def illust_recommended_request(self, offset) -> 'Json':
        """Mode 6"""
        self._await_login()
        return self._api.illust_recommended(offset=offset)

    # Download
    @funcy.retry(tries=3, errors=(ConnectionError, PixivError))
    def protected_download(self, url, path, name) -> 'IO':
        """Protect api download function with funcy.retry so it doesn't crash"""
        self._await_login()
        self._api.download(url, path=path, name=name)


myapi = APIHandler()
