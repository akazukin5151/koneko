import os
import sys
from abc import ABC, abstractmethod
from collections import namedtuple

from koneko import lscat, TERM


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
    def end_func(self):
        raise NotImplementedError

    def start(self):
        show_images = True
        with TERM.cbreak():
            while True:
                if show_images:
                    self.show_func()
                    print(f'Page {self.current_page} / {self.max_pages}')
                    print("Press 'n' to go to next page, "
                            "'p' to go to the previous page, "
                            "and 'q' to quit")

                ans = TERM.inkey()
                print(ans)

                if ans == 'n' and self.current_page == self.max_pages:
                    print("This is the last cached page!")
                    show_images = False

                elif ans == 'p' and self.current_page == self.condition:
                    print("This is the last page!")
                    show_images = False

                elif ans == 'n':
                    self.current_page += 1
                    show_images = True

                elif ans == 'p':
                    self.current_page -= 1
                    show_images = True

                elif ans == 'q':
                    sys.exit(0)

                else:
                    print("Invalid input!")
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

    def end_func(self):
        self.data = FakeData(self.data.download_path.parent / str(self.current_page))


class ImageLoop(AbstractLoop):
    def __init__(self, root, image):
        # Unique
        self.root = root
        self.image = image
        self.all_images = sorted(os.listdir(root))

        # Base ABC
        self.condition = 0
        self.current_page = 0
        self.max_pages = len(self.all_images) - 1

    def show_func(self):
        lscat.icat(self.root / self.image)

    def end_func(self):
        self.image = self.all_images[self.current_page]
