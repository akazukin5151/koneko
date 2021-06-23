from unittest.mock import Mock, call

import pytest
from pixivpy3 import PixivError

from koneko import api


def raises():
    raise PixivError('message')


def test_api_login_with_token_no_error(monkeypatch):
    testapi = api.APIHandler()
    testapi._login_started = False
    mocked_api = Mock()
    testapi._api = mocked_api

    testapi.start({'refresh_token': 'test'})

    assert mocked_api.method_calls == [call.auth(refresh_token='test')]


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
