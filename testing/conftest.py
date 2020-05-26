"""Running pytest with --icat option will run tests that runs icat
Github CI does not run with --icat option, skipping them"""

import pytest

def pytest_addoption(parser):
    parser.addoption(
        "--icat", action="store_true", default=False, help="run functions with icat"
    )

def pytest_configure(config):
    config.addinivalue_line("markers", "icat: mark tests that runs icat")

def pytest_collection_modifyitems(config, items):
    if config.getoption("--icat"):
        # Do not skip
        return
    skip_icat = pytest.mark.skip(reason="need --icat option to run")
    for item in items:
        if "icat" in item.keywords:
            item.add_marker(skip_icat)
