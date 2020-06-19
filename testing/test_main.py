import pytest
from unittest.mock import Mock

from koneko import main

def test_handle_missing_pics(monkeypatch):
    mocked_path = Mock()
    monkeypatch.setattr('koneko.main.Path', mocked_path)
    monkeypatch.setattr('koneko.main.os.system', lambda x: True)
    main.handle_missing_pics()
