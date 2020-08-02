import os
import sys
from collections import namedtuple
from abc import ABC, abstractmethod

from koneko import utils, lscat, config, TERM, printer, FakeData


def scroll_prompt(cls, data, max_images):
    show = True
    terminal_page = 0
    canvas = None

    if cls is lscat.TrackDownloadsUsers:
        max_scrolls = utils.max_terminal_scrolls(data, False)
    else:
        max_scrolls = utils.max_terminal_scrolls(data, True)

    with TERM.cbreak():
        while True:
            if show:
                utils.exit_if_exist(canvas)
                myslice = utils.slice_images(max_images, terminal_page)
                canvas = lscat.handle_scroll(cls, data, myslice)

            ans = TERM.inkey()
            utils.quit_on_q(ans)

            if ans.name == 'KEY_DOWN' and terminal_page + 1 < max_scrolls:
                terminal_page += 1
                show = True

            elif ans.name == 'KEY_UP' and terminal_page > 0:
                terminal_page -= 1
                show = True

            else:
                print('Out of bounds!')
                show = False


class AbstractLoop(ABC):
    @abstractmethod
    def __init__(self):
        self.max_pages: int
        self.condition: int
        self.current_page: int
        self.scrollable: bool
        # Defined in start
        self.terminal_page: int

    @abstractmethod
    def show_func(self) -> 'IO':
        raise NotImplementedError

    def maybe_show_preview(self) -> 'Maybe[IO]':
        return True

    @abstractmethod
    def max_images_func(self) -> 'Any':
        raise NotImplementedError

    def start(self) -> 'IO':
        show_images = True
        self.terminal_page = 0

        with TERM.cbreak():
            while True:
                if show_images:
                    self.show_func()
                    self.maybe_show_preview()
                    print(f'Page {self.current_page} / {self.max_pages}')
                    print(
                        "n: go to next page, "
                        "p: go to previous page, "
                        "q: quit"
                    )

                ans = TERM.inkey()
                print(ans)
                utils.quit_on_q(ans)

                if ans == 'n' and self.current_page == self.max_pages:
                    print('This is the last cached page!')
                    show_images = False

                elif ans == 'p' and self.current_page == self.condition:
                    print('This is the last page!')
                    show_images = False

                elif ans == 'n':
                    os.system('clear')
                    self.current_page += 1
                    show_images = True

                elif ans == 'p':
                    os.system('clear')
                    self.current_page -= 1
                    show_images = True

                elif (ans.name == 'KEY_DOWN'
                        and self.scrollable
                        and self.terminal_page + 1 < self.max_scrolls):
                    self.terminal_page += 1
                    show_images = True

                elif (ans.name == 'KEY_UP'
                        and self.scrollable
                        and self.terminal_page > 0):
                    self.terminal_page -= 1
                    show_images = True

                else:
                    print('Invalid input!')
                    show_images = False

                if show_images:
                    self.max_images_func()


class GalleryUserLoop(AbstractLoop):
    def __init__(self, data, cls):
        # Unique
        self.cls = cls
        self.data = data
        # Unique, defined in classmethods
        self.max_images: int
        self.max_scrolls: int
        self.myslice: slice
        self.canvas: 'Optional[ueberzug.Canvas]' = None

        # Base ABC
        self.condition = 1
        self.current_page = int(data.download_path.name)
        self.scrollable = config.use_ueberzug() or not config.scroll_display()
        self.max_pages = len(
            [x for x in os.listdir(data.download_path.parent)
             if x.isdigit()]
        )

    @classmethod
    def for_gallery(cls, data, tracker):
        result = cls(data, tracker)
        result.max_images = utils.max_images()
        result.max_scrolls = utils.max_terminal_scrolls(data, True)
        return result

    @classmethod
    def for_user(cls, data, tracker):
        result = cls(data, tracker)
        result.max_images = utils.max_images_user()
        result.max_scrolls = utils.max_terminal_scrolls(data, False)
        return result


    def show_func(self) -> 'IO':
        if self.scrollable:
            self.myslice = utils.slice_images(self.max_images, self.terminal_page)
            self.canvas = lscat.handle_scroll(self.cls, self.data, self.myslice)
        else:
            lscat.show_instant(self.cls, self.data)

    def max_images_func(self):
        utils.exit_if_exist(self.canvas)
        self.data = FakeData(self.data.download_path.parent / str(self.current_page))


class ImageLoop(AbstractLoop):
    def __init__(self, root):
        # Unique
        self.use_ueberzug = config.use_ueberzug()
        self.root = root

        self.all_images = [f for f in sorted(os.listdir(root)) if (root / f).is_file()]
        self.image = self.all_images[0]
        if len(self.all_images) > 1:
            self.FakeData = namedtuple('data', ('download_path', 'page_num'))


        # Defined in self.show_func()
        self.canvas: 'Optional[ueberzug.Canvas]' = None
        # Defined in self.maybe_show_preview()
        self.preview_canvas: 'Optional[ueberzug.Canvas]' = None

        # Base ABC
        self.condition = 0
        self.current_page = 0
        self.max_pages = len(self.all_images) - 1
        self.scrollable = False

    def show_func(self) -> 'IO':
        if self.use_ueberzug:
            self.canvas = lscat.ueberzug_center_align(self.root / self.image)
        else:
            lscat.icat(self.root / self.image)

    def max_images_func(self):
        self.image = self.all_images[self.current_page]
        utils.exit_if_exist(self.canvas)
        utils.exit_if_exist(self.preview_canvas)

    def maybe_show_preview(self) -> 'IO':
        if len(self.all_images) > 1:
            tracker = self._update_tracker()
            loc = TERM.get_location()
            for image in self.all_images[self.current_page + 1:][:4]:
                tracker.update(image)
            printer.move_cursor_xy(loc[0], loc[1])
            self.preview_canvas = tracker.canvas

    def _update_tracker(self) -> 'IO':
        """Unique"""
        data = self.FakeData(self.root, self.current_page)
        return lscat.TrackDownloadsImage(data)
