import os
import sys
from collections import namedtuple
from abc import ABC, abstractmethod

from koneko import utils, lscat, config, TERM, printer, FakeData


def scroll_prompt(cls, data, end):
    # TODO: integrate to koneko prompt (currently only works for lscat app)
    show = True
    counter = 0
    canvas = None
    number_of_images = len(os.listdir(data.download_path))
    max_scrolls = number_of_images // end + 1
    if cls is lscat.TrackDownloadsUsers:
        max_scrolls -= 1

    with TERM.cbreak():
        while True:
            if show:
                if canvas:
                    canvas.__exit__()
                myslice = slice(end*counter, end*(counter+1))
                canvas = lscat.handle_scroll(cls, data, myslice)

            ans = TERM.inkey()
            # TODO: extract out all check quit things in the entire codebase
            if ans == 'q':
                sys.exit(0)

            elif ans.name == 'KEY_DOWN' and counter + 1 < max_scrolls:
                counter += 1
                show = True

            elif ans.name == 'KEY_UP' and counter > 0:
                counter -= 1
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

    @abstractmethod
    def show_func(self) -> 'IO':
        raise NotImplementedError

    def maybe_show_preview(self) -> 'Maybe[IO]':
        return True

    @abstractmethod
    def end_func(self) -> 'Any':
        raise NotImplementedError

    def start(self) -> 'IO':
        show_images = True
        self.counter = 0

        with TERM.cbreak():
            while True:
                if show_images:
                    self.response = self.show_func()
                    self.maybe_show_preview()
                    print(f'Page {self.current_page} / {self.max_pages}')
                    print(
                        "n: go to next page, "
                        "p: go to previous page, "
                        "q: quit"
                    )

                ans = TERM.inkey()
                print(ans)

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

                elif ans == 'q':
                    sys.exit(0)

                elif (ans.name == 'KEY_DOWN'
                        and self.scrollable
                        and self.counter + 1 < self.max_scrolls):
                    self.counter += 1
                    show_images = True

                elif (ans.name == 'KEY_UP'
                        and self.scrollable
                        and self.counter > 0):
                    self.counter -= 1
                    show_images = True

                else:
                    print('Invalid input!')
                    show_images = False

                if show_images:
                    self.end_func()


class GalleryUserLoop(AbstractLoop):
    def __init__(self, data, cls):
        # Unique
        self.cls = cls
        self.data = data
        # Unique, defined in classmethods
        self.end: int
        self.max_scrolls: int
        self.myslice: slice

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
        number_of_images = len(os.listdir(data.download_path))
        result.end = utils.max_images()
        result.max_scrolls = number_of_images // result.end + 1
        result.myslice = slice(None, result.end)
        return result

    @classmethod
    def for_user(cls, data, tracker):
        result = cls(data, tracker)
        number_of_images = len(os.listdir(data.download_path))
        result.end = utils.max_images_user()
        result.max_scrolls = number_of_images // result.end
        result.myslice = slice(None, result.end)
        return result


    def show_func(self) -> 'IO':
        if self.scrollable:
            lscat.handle_scroll(self.cls, self.data, self.myslice)
        else:
            lscat.show_instant(self.cls, self.data)

    def end_func(self):
        self.myslice = slice(self.end*self.counter, self.end*(self.counter+1))
        self.data = FakeData(self.data.download_path.parent / str(self.current_page))


class ImageLoop(AbstractLoop):
    def __init__(self, root, image):
        # Unique
        self.use_ueberzug = config.use_ueberzug()
        self.root = root
        self.image = image
        self.all_images = sorted(os.listdir(root))
        if len(self.all_images) > 1:
            self.FakeData = namedtuple('data', ('download_path', 'page_num'))

        # Base ABC
        self.condition = 0
        self.current_page = 0
        self.max_pages = len(self.all_images) - 1
        self.scrollable = False

    def show_func(self) -> 'IO':
        if self.use_ueberzug:
            return lscat.ueberzug(self.root / self.image)
        else:
            lscat.icat(self.root / self.image)

    def end_func(self):
        self.image = self.all_images[self.current_page]
        if self.response:
            self.response.__exit__()

    def update_tracker(self) -> 'IO':
        """Unique"""
        data = self.FakeData(self.root, self.current_page)
        return lscat.TrackDownloadsImage(data)

    def maybe_show_preview(self) -> 'IO':
        if len(self.all_images) > 1:
            tracker = self.update_tracker()
            loc = TERM.get_location()
            for image in self.all_images[self.current_page + 1:][:4]:
                tracker.update(image)
            printer.move_cursor_xy(loc[0], loc[1])
