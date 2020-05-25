"""The default image renderer for koneko"""
import os
import threading
from abc import ABC

from pixcat import Image
from blessed import Terminal

from koneko.pure import cd
from koneko import utils

TERM = Terminal()

def xcoords(term_width, img_width=18, padding=2, offset=0):
    """Generates the x-coord for each column to pass into pixcat
    If img_width == 18 and 90 > term_width > 110, there will be five columns,
    with spaces of (2, 20, 38, 56, 74)
    Meaning the first col has x-coordinates 2 and second col of 20
    """
    number_of_columns = round(term_width / (img_width + padding))
    return [col % number_of_columns * img_width + padding + offset
            for col in range(number_of_columns)]

def ycoords(term_height, img_height=8, padding=1):
    """Generates the y-coord for each row to pass into pixcat
    If img_height == 8 and 27 > term_height >= 18, there will be two rows,
    with spaces of (0, 9)
    Meaning the first row has y-coordinates 0 and second row of 9
    """
    number_of_rows = term_height // (img_height + padding)
    return [row * (img_height + padding)
            for row in range(number_of_rows)]


def icat(args):
    os.system(f'kitty +kitten icat --silent {args}')

def show_instant(cls, download_path, data, check_noprint=False):
    tracker = cls(download_path, data)
    list(map(tracker.update, os.listdir(download_path)))

    if check_noprint and not utils.check_noprint():
        print(' ' * 8, 1, ' ' * 15, 2, ' ' * 15, 3, ' ' * 15, 4, ' ' * 15, 5, '\n')


class Tracker(ABC):
    def __init__(self, path):
        self.path = path
        self._downloaded = []
        self._lock = threading.Lock()
        self._counter = 0
        self.orders: 'List[int]'  # noqa: F821
        self.generator: 'Generator[string]'  # noqa: F821

    def update(self, new):
        with self._lock:
            self._downloaded.append(new)

        self._inspect()

    def _inspect(self):
        """
        images 0-29 are artist profile pics
        images 30-119 are previews, 3 for each artist
        so the valid order is:
        0, 30, 31, 32, 1, 33, 34, 35, 2, 36, 37, 38, ...
        """
        next_num = self.orders[self._counter]
        numlist = [int(f[:3]) for f in self._downloaded]

        if next_num in numlist:
            pic = self._downloaded[numlist.index(next_num)]
            # Display page
            if next_num == 0:
                os.system('clear')
            self.generator.send(pic)

            self._counter += 1
            self._downloaded.remove(pic)
            numlist.remove(next_num)
            if self._downloaded:
                self._inspect()

class TrackDownloads(Tracker):
    """For gallery modes (1 & 5)"""
    def __init__(self, path):
        super().__init__(path)
        self.orders = list(range(30))
        self.generator = generate_page(path)
        self.generator.send(None)

class TrackDownloadsUsers(Tracker):
    """For user modes (3 & 4)"""
    def __init__(self, path, data):
        super().__init__(path)
        noprint = utils.check_noprint()
        self.orders = generate_orders(data.total_imgs, data.splitpoint)
        self.generator = generate_users(path, noprint)
        self.generator.send(None)

def generate_page(path):
    """Given number, calculate its coordinates and display it, then yield"""
    left_shifts = xcoords(TERM.width)
    rowspaces = ycoords(TERM.height)
    while True:
        # Release control. When _inspect() sends another image,
        # assign to the variables and display it again
        image = yield

        number = int(image.split('_')[0])
        x = number % 5
        y = number // 5

        if number % 10 == 0 and number != 0:
            print('\n' * 25)

        with cd(path):
            Image(image).thumbnail(310).show(
                align='left', x=left_shifts[x], y=rowspaces[(y % 2)]
            )

def generate_users(path, noprint=False):
    preview_xcoords = xcoords(TERM.width, offset=1)[-3:]
    os.system('clear')

    while True:
        # Wait for artist pic
        a_img = yield
        artist_name = a_img.split('.')[0].split('_')[-1]
        number = a_img.split('_')[0][1:]
        message = ''.join([number, '\n', ' ' * 18, artist_name])

        if not noprint:
            # Print the message (artist name)
            print(' ' * 18, message)
        print('\n' * 20)  # Scroll to new 'page'

        with cd(path):
            # Display artist profile pic
            Image(a_img).thumbnail(310).show(align='left', x=2, y=0)

            # Display the three previews
            i = 0                   # Always resets for every artist
            while i < 3:            # Every artist has only 3 previews
                p_img = yield       # Wait for preview pic
                Image(p_img).thumbnail(310).show(align='left', y=0,
                                                 x=preview_xcoords[i])
                i += 1

def generate_orders(total_pics, artists_count):
    """
    images 0-29 are artist profile pics
    images 30-119 are previews, 3 for each artist
    so the valid order is:
    0, 30, 31, 32, 1, 33, 34, 35, 2, 36, 37, 38, ...
    a, p,  p,  p,  a, p,  p,  p,  a, ...
    """
    artist = list(range(artists_count))
    prev = list(range(artists_count, total_pics))
    order = []
    a,p = 0,0

    for i in range(total_pics):
        if i % 4 == 0:
            order.append(artist[a])
            a += 1
        else:
            order.append(prev[p])
            p += 1

    return order


class TrackDownloadsImage(Tracker):
    """Experimental"""
    def __init__(self, path):
        super().__init__(path)
        self.orders = list(range(1,30))
        self.generator = generate_previews(path)
        self.generator.send(None)

    def _inspect(self):
        next_num = self.orders[self._counter]
        numlist = [int(f.split('_')[1].replace('p', '')) for f in self._downloaded]

        if next_num in numlist:
            pic = self._downloaded[numlist.index(next_num)]
            # Display page
            if next_num == 0:
                os.system('clear')
            self.generator.send(pic)

            self._counter += 1
            self._downloaded.remove(pic)
            numlist.remove(next_num)
            if self._downloaded:
                self._inspect()

def generate_previews(path):
    """Experimental"""
    rowspaces = ycoords(TERM.height)
    left_shifts = xcoords(TERM.width)
    _xcoords = (left_shifts[0], left_shifts[-1])

    i = 0
    while True:
        # Release control. When _inspect() sends another image,
        # assign to the variables and display it again
        image = yield
        i += 1

        number = int(image.split('_')[1].replace('p', '')) - 1
        y = number % 2
        if i <= 2:
            x = 0
        else:
            x = 1

        with cd(path):
            Image(image).thumbnail(310).show(
                align='left', x=_xcoords[x], y=rowspaces[y]
            )

if __name__ == '__main__':
    from koneko import KONEKODIR
#    import time  # for time.sleep(0.1) in for loop version of map
#    import random  # for random.shuffle(imgs)

    # Use whichever mode you pasted into the test dir
    tracker = TrackDownloadsUsers(KONEKODIR / 'test')
    #tracker = TrackDownloads(KONEKODIR / 'test')
    imgs = os.listdir(KONEKODIR / 'test')
    list(map(tracker.update, imgs))

