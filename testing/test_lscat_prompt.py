from pathlib import Path
from abc import ABC, abstractmethod
from unittest.mock import Mock, call

import pytest
from blessed.keyboard import Keystroke

from koneko import lscat_prompt, lscat
from conftest import CustomExit, raises_customexit


class FakeInKey(ABC):
    """Fakes the Keystroke constructor in blessed. See:
    https://github.com/jquast/blessed/blob/83748bd9e97f9f2fd849588e677d541e162e3fc6/tests/test_keyboard.py#L98
    """
    def __init__(self):
        self.code = True

    @abstractmethod
    def __call__(self):
        raise NotImplementedError


def gallery_setup(monkeypatch):
    monkeypatch.setattr('koneko.lscat_prompt.os.listdir', lambda x: x)
    monkeypatch.setattr('koneko.config.nrows_config', lambda: 6)
    monkeypatch.setattr('koneko.config.ncols_config', lambda: 5)
    monkeypatch.setattr('koneko.utils.max_terminal_scrolls', lambda *a: 2)

    mocked_data = Mock()
    mocked_data.download_path.name = 1
    mocked_data.download_path.parent = ['0', '1']

    return (
        mocked_data,
        lscat_prompt.GalleryUserLoop.for_gallery(mocked_data)
    )


def image_setup(monkeypatch, tmp_path):
    for image in range(4):
        (tmp_path / str(image)).touch()
    return lscat_prompt.ImageLoop(tmp_path)



def test_scroll_prompt(monkeypatch, patch_cbreak):
    class FakeInKeyNew(FakeInKey):
        def __call__(self):
            return Keystroke(ucs='q', code=1, name='q')

    fake_inkey = FakeInKeyNew()
    monkeypatch.setattr('koneko.lscat_prompt.TERM.inkey', fake_inkey)
    monkeypatch.setattr('koneko.utils.quit_on_q', raises_customexit)
    monkeypatch.setattr('koneko.utils.max_terminal_scrolls', lambda *a: 2)
    monkeypatch.setattr('koneko.lscat.handle_scroll', lambda *a: None)

    with pytest.raises(CustomExit):
        assert lscat_prompt.scroll_prompt(lscat.TrackDownloads, Mock(), 10)


def test_gallery_user_loop_init_for_gallery(monkeypatch, patch_cbreak):
    _, p = gallery_setup(monkeypatch)
    assert p.max_images == 6 * 5
    assert p.max_scrolls == 2
    assert p.current_page == 1


def test_gallery_user_loop_init_for_user(monkeypatch, patch_cbreak):
    monkeypatch.setattr('koneko.lscat_prompt.os.listdir', lambda x: x)
    monkeypatch.setattr('koneko.config.nrows_config', lambda: 6)
    monkeypatch.setattr('koneko.utils.max_terminal_scrolls', lambda *a: 2)

    mocked_data = Mock()
    mocked_data.download_path.name = 1
    mocked_data.download_path.parent = ['0', '1']
    p = lscat_prompt.GalleryUserLoop.for_user(mocked_data)
    assert p.max_images == 6 * 4  # 4 previews
    assert p.max_scrolls == 2
    assert p.current_page == 1


@pytest.mark.parametrize('letter', ('n', 'p'))
def test_gallery_user_loop_keys(monkeypatch, patch_cbreak, letter):
    class FakeInKeyNew(FakeInKey):
        def __call__(self):
            return Keystroke(ucs=letter, code=1, name=letter)

    fake_inkey = FakeInKeyNew()
    monkeypatch.setattr('koneko.lscat_prompt.TERM.inkey', fake_inkey)
    monkeypatch.setattr('koneko.lscat_prompt.os.system', lambda x: True)

    _, p = gallery_setup(monkeypatch)
    p.show_func = Mock()
    p.report = Mock()
    p.end_func = raises_customexit
    p.condition = p.current_page + 1  # For 'p'

    with pytest.raises(CustomExit):
        p.start()


def test_gallery_user_loop_key_down(monkeypatch, patch_cbreak):
    class FakeInKeyNew(FakeInKey):
        def __call__(self):
            return Keystroke(ucs='', code=1, name='KEY_DOWN')

    fake_inkey = FakeInKeyNew()
    monkeypatch.setattr('koneko.lscat_prompt.TERM.inkey', fake_inkey)
    monkeypatch.setattr('koneko.lscat_prompt.os.system', lambda x: True)

    _, p = gallery_setup(monkeypatch)
    p.show_func = Mock()
    p.report = Mock()
    p.end_func = raises_customexit
    p.scrollable = True

    with pytest.raises(CustomExit):
        p.start()


def test_gallery_user_loop_show_func_scrollable(monkeypatch):
    mocked_data, p = gallery_setup(monkeypatch)
    p.maybe_show_preview = raises_customexit
    p.scrollable = True

    lscat_mock = Mock()
    monkeypatch.setattr('koneko.lscat.handle_scroll', lscat_mock)

    with pytest.raises(CustomExit):
        p.start()

    assert lscat_mock.call_args_list == [
        call(lscat.TrackDownloads, mocked_data, slice(0, 30, None))
    ]


def test_gallery_user_loop_show_func_not_scrollable(monkeypatch):
    mocked_data, p = gallery_setup(monkeypatch)
    p.maybe_show_preview = raises_customexit
    p.scrollable = False

    lscat_mock = Mock()
    monkeypatch.setattr('koneko.lscat.show_instant', lscat_mock)

    with pytest.raises(CustomExit):
        p.start()

    assert lscat_mock.call_args_list == [
        call(lscat.TrackDownloads, mocked_data)
    ]


def test_gallery_user_loop_end_func(monkeypatch):
    _ , p = gallery_setup(monkeypatch)
    p.images = ['1', '2']
    p.data.download_path.parent = Path('parent')

    lscat_mock = Mock()
    monkeypatch.setattr('koneko.lscat.api.hide_all', lscat_mock)

    p.end_func()

    assert lscat_mock.call_args_list == [call(['1', '2'])]
    assert p.data.download_path == Path('parent/1')


def test_image_loop_init(monkeypatch, tmp_path):
    p = image_setup(monkeypatch, tmp_path)
    assert p.all_images == [str(n) for n in range(4)]
    assert p.image_path == '0'
    assert 'download_path' in dir(p.FakeData)
    assert 'page_num' in dir(p.FakeData)
    assert p.max_pages == 3


def test_image_loop_show_func(monkeypatch, tmp_path):
    p = image_setup(monkeypatch, tmp_path)
    mocked_api = Mock()
    monkeypatch.setattr('koneko.lscat.api', mocked_api)

    p.show_func()
    assert mocked_api.method_calls == [call.show_center(tmp_path / '0')]
    assert p.image == mocked_api.show_center()


def test_image_loop_end_func(monkeypatch, tmp_path):
    p = image_setup(monkeypatch, tmp_path)
    p.image = 'sample'
    p.preview_images = range(4)
    mocked_api = Mock()
    monkeypatch.setattr('koneko.lscat.api', mocked_api)

    p.end_func()
    assert mocked_api.method_calls == [
        call.hide('sample'),
        call.hide_all(range(4)),
    ]
    assert p.image_path == '0'


def test_image_loop_maybe_show_preview(monkeypatch, tmp_path):
    p = image_setup(monkeypatch, tmp_path)
    mocked_tracker = Mock()
    monkeypatch.setattr('koneko.lscat.TrackDownloadsImage', mocked_tracker)
    monkeypatch.setattr('koneko.lscat_prompt.TERM.get_location', lambda: (0, 0))
    monkeypatch.setattr('koneko.printer.move_cursor_xy', lambda *a: True)

    p.maybe_show_preview()
    assert mocked_tracker.mock_calls == [
        call(p.FakeData(tmp_path, 0)),
        call().update('1'),
        call().update('2'),
        call().update('3'),
    ]
    assert p.preview_images == mocked_tracker().images
