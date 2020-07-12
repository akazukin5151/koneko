import pytest
import configparser
from pathlib import Path
from contextlib import contextmanager


@pytest.fixture()
def send_enter(monkeypatch):
    monkeypatch.setattr("builtins.input", lambda x: "")

@contextmanager
def fakecbreak():
    try:
        yield
    finally:
        pass


@pytest.fixture
def patch_cbreak(monkeypatch):
    monkeypatch.setattr('koneko.TERM.cbreak', fakecbreak)


class CustomExit(SystemExit):
    """Replaces all expected instances of an exit,
    to ensure that code exits only where this exception is mocked into
    """

def raises_customexit(*args, **kwargs):
    """As lambdas don't allow raise statements, this is a function"""
    raise CustomExit()


@pytest.fixture
def use_test_cfg_path(monkeypatch, tmp_path):
    monkeypatch.setattr('koneko.config.Path.expanduser',
                        lambda x: Path(tmp_path / 'test_config.ini'))

def setup_test_config(path):
    default = """[Credentials]
    username = koneko
    password = mypassword
    id = 1234

    [lscat]
    image_width = 18
    image_height = 8
    image_thumbnail_size = 310
    images_x_spacing = 2
    images_y_spacing = 1
    gallery_print_spacing = 9,17,17,17,17
    users_print_name_xcoord = 18
    gallery_page_spacing = 23
    users_page_spacing = 20

    [misc]
    print_info = on

    [experimental]
    image_mode_previews = off
    """
    config_object = configparser.ConfigParser()
    config_object.read_string(default)

    config_path = (path / 'test_config.ini')
    config_path.touch()
    with open(config_path, 'w') as c:
        config_object.write(c)
    return config_object



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
