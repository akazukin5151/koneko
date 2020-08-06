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
