"""
The default image renderer for koneko.
"""

import os
import fnmatch
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

        assert len(self._pages_list[0]) <= len(self._rowspaces) == self._rows_in_page
        assert len(self._pages_list) <= len(self._page_spaces)

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
                 preview_xcoords=[[40], [58], [75]], number_of_columns=1,
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

if __name__ == '__main__':
    Gallery('/tmp/koneko/2232374/1/')
