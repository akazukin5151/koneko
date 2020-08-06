from unittest.mock import Mock, call

import pytest
from pixivpy3 import PixivError

from koneko import api


def raises():
    raise PixivError('message')


def test_api_login_without_token_no_error(monkeypatch):
    class SubscriptableMock(Mock):
        subscripts = []  # Default mutable, cannot be in global scope
        def __getitem__(self, item):
            self.subscripts.append(item)
            return self

    testapi = api.APIHandler()
    testapi._login_started = False
    mocked_api = SubscriptableMock()
    writer_mock = Mock()
    testapi._api = mocked_api
    testapi._token = None
    monkeypatch.setattr('koneko.files.write_token_file', writer_mock)

    testapi.start({'Username': 'test', 'Password': '1234'})

    assert mocked_api.method_calls == [call.login('test', '1234')]
    assert mocked_api.subscripts == ['response', 'refresh_token']
    assert writer_mock.call_args_list == [call(api.myapi._token_file, mocked_api.login())]


def test_api_login_with_token_no_error(monkeypatch):
    testapi = api.APIHandler()
    testapi._login_started = False
    mocked_api = Mock()
    writer_mock = Mock()
    testapi._api = mocked_api
    testapi._token = 'sample_token'
    monkeypatch.setattr('koneko.files.write_token_file', writer_mock)

    testapi.start({'Username': 'test', 'Password': '1234'})

    assert mocked_api.method_calls == [call.auth(refresh_token='sample_token')]


def test_api_login_with_token_token_error_uses_normal_method(monkeypatch):
    class SubscriptableMock(Mock):
        subscripts = []  # Default mutable, cannot be in global scope
        def __getitem__(self, item):
            self.subscripts.append(item)
            return self

    testapi = api.APIHandler()
    testapi._login_started = False
    mocked_api = SubscriptableMock()
    writer_mock = Mock()
    testapi._api = mocked_api
    testapi._api.auth = lambda **k: raises()
    testapi._token = "sample_token_that_doesn't_work"
    monkeypatch.setattr('koneko.files.write_token_file', writer_mock)

    testapi.start({'Username': 'test', 'Password': '1234'})

    assert mocked_api.method_calls == [call.login('test', '1234')]
    assert mocked_api.subscripts == ['response', 'refresh_token']
    assert writer_mock.call_args_list == [call(api.myapi._token_file, mocked_api.login())]


def test_api_login_without_token_failed(monkeypatch, capsys):
    testapi = api.APIHandler()
    testapi._login_started = False
    testapi._api = Mock()
    testapi._api.login = lambda *a: raises()
    testapi._token = None

    testapi.start({'Username': 'test', 'Password': '1234'})

    assert capsys.readouterr().out == 'Login failed! Please correct your credentials in ~/.config/koneko/config.ini\nmessage\nPress \'q\' and enter to exit\n'


def test_api_mode1(monkeypatch):
    mocked_api = Mock()
    mock_thread = Mock()

    testapi = api.APIHandler()
    testapi._api = mocked_api
    testapi._api_thread = mock_thread
    testapi._login_started = True
    testapi._login_done = False

    testapi.artist_gallery(123, 0)

    assert mocked_api.mock_calls == [call.user_illusts(123, offset=0)]
    assert mock_thread.mock_calls == [call.join()]
    assert testapi._login_done == True


def test_api_mode2(monkeypatch):
    mocked_api = Mock()
    mock_thread = Mock()

    testapi = api.APIHandler()
    testapi._api = mocked_api
    testapi._api_thread = mock_thread
    testapi._login_started = True
    testapi._login_done = False

    testapi.protected_illust_detail(123)

    assert mocked_api.mock_calls == [call.illust_detail(123)]
    assert mock_thread.mock_calls == [call.join()]
    assert testapi._login_done == True


def test_api_mode3(monkeypatch):
    mocked_api = Mock()
    mock_thread = Mock()

    testapi = api.APIHandler()
    testapi._api = mocked_api
    testapi._api_thread = mock_thread
    testapi._login_started = True
    testapi._login_done = False

    testapi.following_user_request(123, 'private', 0)

    assert mocked_api.mock_calls == [call.user_following(123, restrict='private', offset=0)]
    assert mock_thread.mock_calls == [call.join()]
    assert testapi._login_done == True


def test_api_mode4(monkeypatch):
    mocked_api = Mock()
    mock_thread = Mock()

    testapi = api.APIHandler()
    testapi._api = mocked_api
    testapi._api_thread = mock_thread
    testapi._login_started = True
    testapi._login_done = False

    testapi.search_user_request('searchstring', 0)

    assert mocked_api.mock_calls == [call.search_user('searchstring', offset=0)]
    assert mock_thread.mock_calls == [call.join()]
    assert testapi._login_done == True


def test_api_mode5(monkeypatch):
    mocked_api = Mock()
    mock_thread = Mock()

    testapi = api.APIHandler()
    testapi._api = mocked_api
    testapi._api_thread = mock_thread
    testapi._login_started = True
    testapi._login_done = False

    testapi.illust_follow_request('private', 0)

    assert mocked_api.mock_calls == [call.illust_follow(restrict='private', offset=0)]
    assert mock_thread.mock_calls == [call.join()]
    assert testapi._login_done == True


def test_api_mode15(monkeypatch):
    mocked_api = Mock()
    mock_thread = Mock()

    testapi = api.APIHandler()
    testapi._api = mocked_api
    testapi._api_thread = mock_thread
    testapi._login_started = True
    testapi._login_done = False

    testapi.illust_related_request(123, 0)

    assert mocked_api.mock_calls == [call.illust_related(illust_id=123, offset=0)]
    assert mock_thread.mock_calls == [call.join()]
    assert testapi._login_done == True


def test_api_mode6(monkeypatch):
    mocked_api = Mock()
    mock_thread = Mock()

    testapi = api.APIHandler()
    testapi._api = mocked_api
    testapi._api_thread = mock_thread
    testapi._login_started = True
    testapi._login_done = False

    testapi.illust_recommended_request(0)

    assert mocked_api.mock_calls == [call.illust_recommended(offset=0)]
    assert mock_thread.mock_calls == [call.join()]
    assert testapi._login_done == True


def test_api_protected_download(monkeypatch):
    mocked_api = Mock()
    mock_thread = Mock()

    testapi = api.APIHandler()
    testapi._api = mocked_api
    testapi._api_thread = mock_thread
    testapi._login_started = True
    testapi._login_done = False

    testapi.protected_download('url', 'path', 'name')

    assert mocked_api.mock_calls == [call.download('url', path='path', name='name')]
    assert mock_thread.mock_calls == [call.join()]
    assert testapi._login_done == True
