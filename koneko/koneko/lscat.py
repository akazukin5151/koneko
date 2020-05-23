"""
The default image renderer for koneko.
"""

import os
import fnmatch
import threading
from abc import ABC, abstractmethod

import funcy
import cytoolz
from pixcat import Image
from blessed import Terminal

from koneko.pure import cd

# - Pure functions
def is_image(myfile):
    if fnmatch.fnmatch(myfile, '*.jpg') or fnmatch.fnmatch(myfile, '*.png'):
        return True
    return False


def filter_jpg(path):
    with cd(path):
        return sorted(filter(is_image, os.listdir('.')))


@cytoolz.curry
def xcoord(image_number, number_of_columns, width, increment=2):
    return image_number % number_of_columns * width + increment


def number_prefix(myfile):
    return int(myfile.split('_')[0])


def xcoords(term_width, img_width=18, padding=2):
    """Generates the x-coord for each column to pass into pixcat
    If img_width == 18 and 90 > term_width > 110, there will be five columns,
    with spaces of (2, 20, 38, 56, 74)
    Meaning the first col has x-coordinates 2 and second col of 20
    """
    number_of_columns = round(term_width / (img_width + padding))
    return [col % number_of_columns * img_width + padding
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


# Impure functions
def icat(args):
    os.system(f'kitty +kitten icat --silent {args}')

@funcy.ignore(IndexError, TypeError)
def display_page(page, rowspaces, cols, left_shifts, path):
    """For every row, display the images in the row by iterating over every col"""
    with cd(path):
        for (index, space) in enumerate(rowspaces):
            for col in cols:
                Image(page[index][col]).thumbnail(310).show(
                    align='left', x=left_shifts[col], y=space
                )


class View(ABC):
    """
    The reason for using pages is because every time something in a different
    row is displayed, the entire terminal shifts.
    So if you try to naively plot every image as it loads, it would result
    in a cascading gallery l
                            i
                             k
                              e
                                this.
    Hence, the need to plot each row of images in order
    """
    def __init__(self, path, number_of_columns, rowspaces, page_spaces, rows_in_page):
        self._path = path
        self._number_of_columns = number_of_columns
        self._rowspaces = rowspaces
        self._page_spaces = page_spaces
        self._rows_in_page = rows_in_page

        self._cols = range(self._number_of_columns)
        total_width = 90
        width = total_width // self._number_of_columns

        file_list = filter_jpg(path)
        calc = xcoord(number_of_columns=self._number_of_columns, width=width)
        self._left_shifts = list(map(calc, self._cols))

        # partitions list of files into tuples with len == number_of_columns
        # so each row will contain 5 files, if number_of_columns == 5
        # [(file1, file2, ... , file5), (file6, ... , file10), ...]
        each_row = cytoolz.partition_all(self._number_of_columns, file_list)

        # each page has `rows_in_page` rows. every row is grouped with another.
        # [(row1, row2), (row3, row4), ...]
        # where row1 == (file1, file2, ...)
        self._pages_list = cytoolz.partition(self._rows_in_page, each_row, pad=None)
        self._pages_list = list(self._pages_list)

        #assert len(self._pages_list[0]) <= len(self._rowspaces) == self._rows_in_page
        #assert len(self._pages_list) <= len(self._page_spaces)

    @abstractmethod
    def render(self):
        raise NotImplementedError


class Gallery(View):
    """
    Each page has 2 rows by default. A page means printing blank lines to move
    the cursor down (and the terminal screen). The number of blank lines to
    print for each page is given by page_spaces.

    Parameters
    ========
    page_spaces : tuple of ints
        Vertical spacing between pages. Number of newlines to print for every page
        len must be >= number of pages
    """

    def __init__(self, path, page_spaces=(26, 24, 24)):
        # Only to set default arguments here, no overriding
        TERM = Terminal()
        number_of_columns = round(TERM.width / (18 + 2))  # Temp xcoords(TERM.width)
        rowspaces = ycoords(TERM.height)
        rows_in_page = TERM.height // (8 + 1)  # Temp
        # TODO: no need to directly calculate left shifts
        super().__init__(path, number_of_columns, rowspaces, page_spaces, rows_in_page)

    @funcy.ignore(IndexError)
    def render(self):
        os.system('clear')
        for (i, page) in enumerate(self._pages_list):
            print('\n' * self._page_spaces[i])  # Scroll to new 'page'
            display_page(page, self._rowspaces, self._cols, self._left_shifts,
                         self._path)

        print(' ' * 8, 1, ' ' * 15, 2, ' ' * 15, 3, ' ' * 15, 4, ' ' * 15, 5, '\n')


class Card(View):
    """
    Display each image (artist profile pic) in one row, their name, and three
    preview images alongside.

    Special parameters
    ========
    preview_xcoords : list of list of int
        For printing previews next to artists. len == 3 (three previews)
        len of inner list == 1 (one column only, only one int needed)
    preview_paths : list of str
        Path to where the preview images are downloaded to
        len must be 3, for three previews. The number of images in each dir/path
        must be == number of pages == len(page_spaces) == number of images
    messages : list of str
        List of text to print next to the images. Only for when rows_in_page = 1
        len must be >= rows_in_page
    """

    def __init__(self, path, preview_paths, messages,
                 preview_xcoords=((40,), (58,), (75,)), number_of_columns=1,
                 rowspaces=(0,), page_spaces=(20,) * 30, rows_in_page=1):
        # Set defaults ^^^
        self._preview_paths = preview_paths
        self._messages = messages
        self._preview_xcoords = preview_xcoords
        self._preview_images: 'List[List[str]]'
        super().__init__(path, number_of_columns, rowspaces, page_spaces,
                         rows_in_page)

    @funcy.ignore(IndexError)
    def render(self):
        assert self._rows_in_page == 1
        assert len(self._messages) >= self._rows_in_page

        self._preview_images = list(
            cytoolz.partition_all(3, sorted(os.listdir(self._preview_paths)))
        )

        os.system('clear')
        for (i, page) in enumerate(self._pages_list):
            # Print the message (artist name) first
            print('\n' * 2)
            print(' ' * 18, self._messages[i])
            print('\n' * self._page_spaces[i])  # Scroll to new 'page'

            # Display artist profile pic
            display_page(page, self._rowspaces, self._cols, self._left_shifts,
                         self._path)

            # Display the three previews
            for (j, coord) in enumerate(self._preview_xcoords):
                display_page(((self._preview_images[i][j],),), self._rowspaces,
                             self._cols, coord, self._preview_paths)

class Tracker(ABC):
    def __init__(self, path):
        self.path = path
        self._downloaded = []
        self._lock = threading.Lock()
        self._counter = 0
        self.orders: 'List[int]'
        self.generator: 'Generator[string]'

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
    def __init__(self, path):
        super().__init__(path)
        self.orders = list(range(30))
        self.generator = generate_page(path)
        self.generator.send(None)

class TrackDownloadsUsers(Tracker):
    def __init__(self, path):
        super().__init__(path)
        self.orders = generate_orders(120, 30)
        self.generator = generate_users(path)
        self.generator.send(None)

def generate_page(path):
    """ Given number, calculate its coordinates and display it, then yield
    """
    # TODO: These numbers are generated from the above View ABC
    left_shifts = (2,20,38,56,74)
    rowspaces = (0, 9)
    #page_spaces=(26, 24, 24)
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

def generate_users(path, rowspaces=(0,), cols=range(1), artist_xcoords=(2,),
                   preview_xcoords=((40,), (58,), (75,))):

    os.system('clear')
    while True:
        # Wait for artist pic
        a_img = yield
        artist_name = a_img.split('.')[0].split('_')[-1]
        number = a_img.split('_')[0][1:]
        message = ''.join([number, '\n', ' ' * 18, artist_name])
        a_img = ((a_img,),)

        # Print the message (artist name)
        print('\n' * 2)
        print(' ' * 18, message)
        print('\n' * 20)  # Scroll to new 'page'

        # Display artist profile pic
        display_page(a_img, rowspaces, cols, artist_xcoords, path)

        # Display the three previews
        i = 0  # Always resets for every artist
        while i < 3:  # Every artist has only 3 previews
            p_img = yield  # Wait for preview pic
            p_img = ((p_img,),)
            display_page(p_img, rowspaces, cols, preview_xcoords[i], path)
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
    rowspaces = (0, 9)
    xcoords = (2, 75)
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
                align='left', x=xcoords[x], y=rowspaces[y]
            )

if __name__ == '__main__':
    from koneko import KONEKODIR
    Gallery(KONEKODIR / '2232374' / '1').render()
#    import time
#    import random
#
#    imgs = os.listdir(KONEKODIR / 'following' / 'test')
#    random.shuffle(imgs)
#
#    messages = ['test'] * len(imgs)
#    tracker = TrackDownloads(KONEKODIR / 'following' / 'test')
#
#    # Simulates downloads finishing and updating the tracker
#    # Which will display the pictures in the correct order, waiting if needed
#    for img in imgs:
#        tracker.update(img)
#        time.sleep(0.1)
