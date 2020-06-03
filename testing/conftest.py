import pytest


@pytest.fixture()
def turn_off_print(monkeypatch):
    monkeypatch.setattr("builtins.print", lambda *a, **k: "")


def pytest_addoption(parser):
    parser.addoption(
        "--inte", action="store_true", default=False, help="run integration tests"
    )

def pytest_configure(config):
    config.addinivalue_line("markers", "integration: run integration tests")

def pytest_collection_modifyitems(config, items):
    if config.getoption("--inte"):
        # Do not skip
        return
    skip = pytest.mark.skip(reason="need --inte option to run")
    for item in items:
        if "integration" in item.keywords:
            item.add_marker(skip)
