import pytest
from unittest.mock import Mock

from koneko import main

def test_handle_missing_pics(monkeypatch):
    main.handle_missing_pics()
