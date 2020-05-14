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


# Impure functions
@funcy.ignore(IndexError, TypeError)
def display_page(page, rowspaces, cols, left_shifts, path):
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
    rowspaces : tuple of ints
        Vertical spacing between (the two) rows.
        len must be >= number of rows
    page_spaces : tuple of ints
        Vertical spacing between pages. Number of newlines to print for every page
        len must be >= number of pages
    rows_in_page : int
        Number of rows in each page
    print_rows : bool
        Whether to print row numbers in the bottom

    Info
    ========
    left_shifts : list of ints
        Horizontal position of the image
    """

    def __init__(self, path, number_of_columns=5, rowspaces=(0, 9),
                 page_spaces=(26, 24, 24), rows_in_page=2):
        # Only to set default arguments here, no overriding
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

class NewCard(Card):
    # TODO: detach from Card
    @funcy.ignore(IndexError)
    def render(self):
        os.system('clear')
        while True:
            # Wait for artist pic
            a_img = yield
            #breakpoint()
            artist_name = a_img.split('.')[0].split('_')[-1]
            number = a_img.split('_')[0][1:]
            message = ''.join([number, '\n', ' ' * 18, artist_name])
            a_img = ((a_img,),)

            # Print the message (artist name)
            print('\n' * 2)
            print(' ' * 18, message)
            print('\n' * 20)  # Scroll to new 'page'

            # Display artist profile pic
            display_page(a_img, self._rowspaces, self._cols, self._left_shifts,
                         self._path)

            # Display the three previews
            j = 0
            while True:
                if j >= 3:
                    break
                coord = self._preview_xcoords[j]
                img = yield  # Wait for preview pic
                img = ((img,),)
                display_page(img, self._rowspaces,
                             self._cols, coord, self._path)
                j += 1


class TrackDownloads:
    def __init__(self, path):
        self.path = path
        self._downloaded = []
        self._lock = threading.Lock()
        self._counter = 0

    def update(self, new):
        with self._lock:
            self._downloaded.append(new)

        self._inspect(new)

    def _inspect(self, new):
        """
        Counter always goes up, so every image to displayed in order
        1. If image that was just submitted (just finished downloading) == counter,
        2. Display it
        3. Increment counter
        4. Remove from the list
        5. Sort
        6. Inspect the first item in the list as if it was just submitted.
            a. If it is == counter, display it, etc
            b. Else, do nothing (continue to accept submissions)

        Example with numbers submitted in this order: (3,8,4,7,0,1,9,5,2,6)
        3
        3,8
        3,8,4
        3,8,4,7         # Do nothing in all above lines
        3,8,4,7,0       # New number == counter, take out 0, display it, sort the rest
        3,4,7,8         # Counter is now 1 , but 1 != 3, so do nothing
        3,4,7,8,1       # New number == counter, so take out 1, display, sort
        3,4,7,8         # Counter is now 2, do nothing
        3,4,7,8,9
        3,4,7,8,9,5
        3,4,7,8,9,5,2   # Take out 2, display, sort
        3,4,5,7,8,9     # Counter is now 3; tds
        4,5,7,8,9       # Counter is now 4; tds
        5,7,8,9         # Counter is now 5; tds
        7,8,9           # Counter is now 6, but 6 != 7, so do nothing
        7,8,9,6         # New number == counter; tds
        7,8,9           # Counter is now 7; tds
        ...             # Done
        """
        number = int(new[:3]) # Only for renamed images
        if number == self._counter:
            # Display page
            if number == 0:
                os.system('clear')
                self.generator = generate_page(new, self.path, 0)
                next(self.generator)
            else:
                self.generator.send((new, number))

            self._counter += 1
            self._downloaded.remove(new)
            self._downloaded.sort()
            if self._downloaded:
                self._inspect(self._downloaded[0])


#@funcy.ignore(IndexError, TypeError)
def generate_page(image, path, number):
    """ Given number, calculate its coordinates and display it, then yield
    For reference, (y-coordinate, x-coordinate) for every image
    (0, 2), (0, 20), (0, 38), (0, 56), (0, 74),
    (9, 2), (9, 29), (9, 38), (9, 56), (9, 74),
    (0, 2), (0, 20), (0, 38), (0, 56), (0, 74),
    (9, 2), (9, 29), (9, 38), (9, 56), (9, 74),
    (0, 2), (0, 20), (0, 38), (0, 56), (0, 74),
    (9, 2), (9, 29), (9, 38), (9, 56), (9, 74),
    """
    # TODO: These numbers are generated from the above View ABC
    left_shifts = (2,20,38,56,74)
    rowspaces = (0, 9)
    #page_spaces=(26, 24, 24)
    with cd(path):
        while True:
            x = number % 5
            y = number // 5

            if number % 10 == 0 and number != 0:
                print('\n' * 25) #self._page_spaces[i])

            Image(image).thumbnail(310).show(
                align='left', x=left_shifts[x], y=rowspaces[(y % 2)]
            )

            # Release control. When _inspect() sends another image,
            # assign to the variables and display it again
            image, number = yield


class TrackDownloadsUsers(TrackDownloads):
    def __init__(self, path):
        super().__init__(path)
        self.orders = generate_orders(120, 30)
        self.generator = NewCard(path, None, None).render()
        self.generator.send(None)

    def _inspect(self, new):  # new is not used
        """
        images 0-29 are artist profile pics
        images 30-119 are previews, 3 for each artist
        so the valid order is:
        0, 30, 31, 32, 1, 33, 34, 35, 2, 36, 37, 38, ...

        Simplified example of this algorithm, with 3 artists & 9 previews total:
        numbers received in this order: [3, 8, 5, 4, 0, 1, 2, 11, 6, 9, 10, 7]
        0-2 are artists and 3-10 are the previews, so the display order is:
        (0, 3, 4, 5, 1, 6, 7, 8, 2, 9, 10, 11)
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
                self._inspect(None)


def generate_orders(total_pics, artists_count):
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


if __name__ == '__main__':
    from koneko import KONEKODIR
    #Gallery(KONEKODIR / '2232374' / '1').render()
    import time
    import random

    imgs = [f for f in os.listdir(KONEKODIR / 'following' / 'test')]
    random.shuffle(imgs)

    messages = ['test'] * len(imgs)
    tracker = TrackDownloadsUsers(KONEKODIR / 'following' / 'test')

    for img in imgs:
        tracker.update(img)
        time.sleep(0.1)
