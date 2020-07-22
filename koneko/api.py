"""Handles all Pixiv API interactions, eg async login, requests"""

import threading

import funcy
from pixivpy3 import PixivError, AppPixivAPI

from koneko import files, utils, KONEKODIR


class APIHandler:
    """Singleton that handles all the API interactions in the program"""
    def __init__(self):
        self._api_thread = threading.Thread(target=self._login)
        self._token_file = KONEKODIR.parent / 'token'
        self._token = files.read_token_file(self._token_file)

        self._api = AppPixivAPI()  # Object to login and request on
        # Set in self.start() (because singleton is instantiated before config)
        self._credentials: 'dict[str, str]'

    @funcy.once
    def start(self, credentials):
        """Start logging in. The only setup entry point that is public"""
        self._credentials = credentials
        self._api_thread.start()

    @funcy.once
    def _await_login(self):
        """Wait for login to finish, then assign PixivAPI session to API"""
        self._api_thread.join()


    def _login(self):
        """Logins to pixiv in the background, using credentials from config file"""
        if self._token:  # Token is set if file found in self.start()
            token_success = self._login_with_token()

        if not self._token or not token_success:
            response = self._login_with_creds()
            if response:
                self._token = response['response']['refresh_token']
                files.write_token_file(self._token_file, self._token)


    def _login_with_token(self) -> bool:
        """Tries to login with saved token to avoid pixiv emails
        Returns True on successful login with token, False otherwise
        """
        try:
            self._api.auth(refresh_token=self._token)
        except PixivError as e:
            return False
        return True


    def _login_with_creds(self) -> 'Optional[dict[dict[str]]]':
        try:
            return self._api.login(
                self._credentials['Username'],
                self._credentials['Password']
            )

        except PixivError as e:
            print('Login failed! Please correct your credentials in '
                  '~/.config/koneko/config.ini')
            print(e)
            print("Press 'q' and enter to exit")


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
        """Mode 15 (1.5 * 10 so it's an int)"""
        self._await_login()
        return self._api.illust_related(illust_id=image_id, offset=offset)

    @funcy.retry(tries=3, errors=(ConnectionError, PixivError))
    @utils.spinner('')
    def illust_recommended_request(self, offset):
        """Mode 6"""
        self._await_login()
        return self._api.illust_recommended(offset=offset)

    # Download
    @funcy.retry(tries=3, errors=(ConnectionError, PixivError))
    def protected_download(self, url, path, name):
        """Protect api download function with funcy.retry so it doesn't crash"""
        self._await_login()
        self._api.download(url, path=path, name=name)


myapi = APIHandler()
