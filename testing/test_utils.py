from unittest.mock import Mock, call
from logging.handlers import RotatingFileHandler

import pytest

from koneko import utils

from testing.page_json import page_json  # isort:skip

page_illusts = page_json['illusts']


@pytest.mark.parametrize('msg', ('', 'message'))
def test_spinner_normal(capsys, msg):
    @utils.spinner(msg)
    def function_to_be_wrapped():
        return 'sentinel'

    assert function_to_be_wrapped() == 'sentinel'
    assert capsys.readouterr().out == f'{msg} |\r \r'


def test_spinner_with_exception(capsys):
    @utils.spinner('')
    def function_to_be_wrapped():
        raise Exception

    with pytest.raises(Exception):
        function_to_be_wrapped()
    assert capsys.readouterr().out == ' |\r \r'


def test_catch_ctrl_c(monkeypatch):
    mocked_system = Mock()
    monkeypatch.setattr('koneko.utils.os.system', mocked_system)

    def function_that_sends_ctrl_c():
        raise KeyboardInterrupt

    utils.catch_ctrl_c(function_that_sends_ctrl_c)()
    assert mocked_system.call_args_list == [call('clear')]


def test_max_terminal_scrolls_gallery(monkeypatch):
    monkeypatch.setattr('koneko.config.ncols_config', lambda: 6)
    monkeypatch.setattr('koneko.config.nrows_config', lambda: 5)
    monkeypatch.setattr('koneko.utils.os.listdir', lambda *a: [0] * 10)
    assert utils.max_terminal_scrolls(Mock(), True) == 10 // (6 * 5) + 1


def test_max_terminal_scrolls_user(monkeypatch):
    monkeypatch.setattr('koneko.config.nrows_config', lambda: 5)
    monkeypatch.setattr('koneko.utils.os.listdir', lambda *a: [0] * 10)
    assert utils.max_terminal_scrolls(Mock(), False) == 10 // (4 * 5)


def test_slice_images():
    assert utils.slice_images(10, 0) == slice(0, 10)
    assert utils.slice_images(10, 1) == slice(10, 20)


def test_find_number_map(monkeypatch):
    monkeypatch.setattr('koneko.config.ncols_config', lambda: 5)
    assert ([utils.find_number_map(x, y)
             for y in range(1, 7)
             for x in range(1, 6)] == list(range(30)))
    assert not utils.find_number_map(0, 100)

    monkeypatch.setattr('koneko.config.ncols_config', lambda: 6)
    assert [utils.find_number_map(x, y)
            for y in range(1, 7)
            for x in range(1, 7)][:30] == list(range(30))


def test_history(monkeypatch, tmp_path):
    test_log = tmp_path / 'history'
    monkeypatch.setattr('koneko.utils.RotatingFileHandler',
                        lambda *a, **k: RotatingFileHandler(test_log))

    # test setup_history_log()
    logger = utils.setup_history_log()
    logger.info('1: 1234')
    logger.info('2: 5678')
    with open(test_log, 'r') as f:
        assert f.read() == '1: 1234\n2: 5678\n'

    # test read_history()
    monkeypatch.setattr('koneko.utils.KONEKODIR', tmp_path)
    assert utils.read_history() == ['1: 1234', '2: 5678']

    # test frequent_history()
    assert utils.frequent_history() == {'1: 1234': 1, '2: 5678': 1}
    assert utils.frequent_history(1) == {'1: 1234': 1}

    # test frequent_history_modes()
    assert (utils.frequent_history_modes(['1', '2'])
            == utils.frequent_history()
            == {'1: 1234': 1, '2: 5678': 1})
    assert utils.frequent_history_modes(['1']) == {'1: 1234': 1}
    assert utils.frequent_history_modes(['2']) == {'2: 5678': 1}
    assert (utils.frequent_history_modes(['3'])
            == utils.frequent_history_modes(['4'])
            == utils.frequent_history_modes(['5'])
            == dict())

    # test format_frequent()
    counter = utils.frequent_history()
    assert utils.format_frequent(counter) == ['1: 1234 (1)', '2: 5678 (1)']


def test_handle_missing_pics():
    utils.handle_missing_pics()


def test_max_images(monkeypatch):
    monkeypatch.setattr('koneko.config.nrows_config', lambda: 2)
    monkeypatch.setattr('koneko.config.ncols_config', lambda: 5)
    assert utils.max_images() == 10
