import os
import sys
from pathlib import Path
from logging.handlers import RotatingFileHandler

import pytest

from koneko import utils

# Lmao python
sys.path.append('testing')

from page_json import page_json  # isort:skip

page_illusts = page_json["illusts"]


def test_find_number_map(monkeypatch):
    monkeypatch.setattr('koneko.utils.ncols_config', lambda: 5)
    assert ([utils.find_number_map(x, y)
             for y in range(1, 7)
             for x in range(1, 6)] == list(range(30)))
    assert not utils.find_number_map(0, 100)

    monkeypatch.setattr('koneko.utils.ncols_config', lambda: 6)
    assert [utils.find_number_map(x, y)
            for y in range(1, 7)
            for x in range(1, 7)][:30] == list(range(30))

def test_cd():
    current_dir = os.getcwd()
    with utils.cd(current_dir):
        testdir = os.getcwd()

    assert testdir == os.getcwd()
    assert os.getcwd() == current_dir


def test_history(monkeypatch):
    test_log = 'testing/history'
    monkeypatch.setattr('koneko.utils.RotatingFileHandler',
                        lambda *a, **k: RotatingFileHandler(test_log))

    # test setup_history_log()
    logger = utils.setup_history_log()
    logger.info('1: 1234')
    logger.info('2: 5678')
    with open(test_log, 'r') as f:
        assert f.read() == '1: 1234\n2: 5678\n'

    try:
        # test read_history()
        monkeypatch.setattr('koneko.utils.KONEKODIR', 'testing')
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

    finally:
        os.system(f'rm {test_log}')


def test_handle_missing_pics():
    utils.handle_missing_pics()
