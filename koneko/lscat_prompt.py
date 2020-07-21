import os
import sys
from abc import ABC, abstractmethod
from collections import namedtuple

from koneko import lscat, TERM, printer


# Duplicated due to circular import
FakeData = namedtuple('data', ('download_path',))

class AbstractLoop(ABC):
    @abstractmethod
    def __init__(self):
        self.max_pages: int
        self.condition: int
        self.current_page: int

    @abstractmethod
    def show_func(self):
        raise NotImplementedError

    @abstractmethod
    def middle(self):
        raise NotImplementedError

    @abstractmethod
    def end_func(self):
        raise NotImplementedError

    def start(self):
        show_images = True
        with TERM.cbreak():
            while True:
                if show_images:
                    self.show_func()
                    self.middle()
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

        # Base ABC
        self.condition = 1
        self.current_page = int(data.download_path.name)
        self.max_pages = len(
            [x for x in os.listdir(data.download_path.parent)
             if x.isdigit()]
        )

    def show_func(self):
        lscat.show_instant(self.cls, self.data)

    def middle(self):
        return True

    def end_func(self):
        self.data = FakeData(self.data.download_path.parent / str(self.current_page))


class ImageLoop(AbstractLoop):
    def __init__(self, root, image):
        # Unique
        self.root = root
        self.image = image
        self.all_images = sorted(os.listdir(root))
        if len(self.all_images) > 1:
            self.FakeData = namedtuple('data', ('download_path', 'page_num'))

        # Base ABC
        self.condition = 0
        self.current_page = 0
        self.max_pages = len(self.all_images) - 1

    def show_func(self):
        lscat.icat(self.root / self.image)

    def end_func(self):
        self.image = self.all_images[self.current_page]

    def update_tracker(self):
        data = self.FakeData(self.root, self.current_page)
        return lscat.TrackDownloadsImage(data)

    def middle(self):
        if len(self.all_images) > 1:
            tracker = self.update_tracker()
            loc = TERM.get_location()
            for image in self.all_images[self.current_page + 1:][:4]:
                tracker.update(image)
            printer.move_cursor_xy(loc[0], loc[1])
