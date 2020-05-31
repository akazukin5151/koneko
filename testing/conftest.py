import pytest


@pytest.fixture()
def turn_off_print(monkeypatch):
    monkeypatch.setattr("builtins.print", lambda *a, **k: "")
