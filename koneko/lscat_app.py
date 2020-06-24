import os
import sys
import time
from copy import copy
from pathlib import Path
from abc import ABC, abstractmethod

from pixcat import Image
from blessed import Terminal

from koneko import KONEKODIR, lscat, config


# Globals
term = Terminal()
# Must make a copy before using this reference
SAMPLE_IMAGE = Image(KONEKODIR.parent / 'pics' / '71471144_p0.png')
PLUS = {'+', '='}
MINUS = {'-', '_'}


# Utility functions used in multiple places
def check_quit(ans):
    if ans == 'q':
        sys.exit(0)

def move_cursor_up(num):
    if num > 0:
        print(f'\033[{num}A', end='', flush=True)

def move_cursor_down(num=1):
    if num > 0:
        print(f'\033[{num}B', end='', flush=True)

def erase_line():
    print('\033[K', end='', flush=True)

def print_cols(spacing, ncols):
    for (idx, space) in enumerate(spacing[:ncols]):
        print(' ' * int(space), end='', flush=True)
        print(idx + 1, end='', flush=True)

def line_width(spacing, ncols):
    return sum(spacing) + ncols

def print_doc(doc):
    os.system('clear')
    bottom = term.height - 7
    move_cursor_down(bottom)
    print(doc)


# More specialised but still small functions
class FakeData:
    def __init__(self, path):
        self.download_path = path

    @classmethod
    def gallery(cls):
        return cls(KONEKODIR / 'testgallery')

    @classmethod
    def user(cls):
        # It needs to have a .koneko file
        return cls(KONEKODIR / 'testuser')


def display_user_row(thumbnail_size, preview_xcoords, padding):
    (copy(SAMPLE_IMAGE)
        .thumbnail(thumbnail_size)
        .show(align='left', x=padding, y=0))

    for px in preview_xcoords:
        (copy(SAMPLE_IMAGE)
            .thumbnail(thumbnail_size)
            .show(align='left', x=px, y=0))


def print_info(message_xcoord):
    print(' ' * message_xcoord, '000', '\n',
          ' ' * message_xcoord, 'Example artist', sep='')


def show_single(x, thumbnail_size):
    img = copy(SAMPLE_IMAGE).thumbnail(thumbnail_size)
    img.show(align='left', x=x, y=0)
    return img

def show_single_y(y, thumbnail_size):
    img = copy(SAMPLE_IMAGE).thumbnail(thumbnail_size)
    img.show(align='left', x=config.xcoords_config()[1], y=y)
    return img


# Main functions that organise work
def main():
    os.system('clear')
    print(*('Welcome to the lscat interactive script',
        '1. Launch koneko configuration assistance',
        '2. Display KONEKODIR / testgallery',
        '3. Display KONEKODIR / testuser',
        '4. Browse a cached dir to display',
        '5. Display a specified path'), sep='\n')

    ans = input('\nPlease select an action: ')
    print('')

    case = {
        '1': config_assistance,
        '2': display_gallery,
        '3': display_user,
        '4': browse_cache,
        '5': display_path
    }

    func = case.get(ans, None)
    if func:
        func()
    else:
        print('Invalid command! Exiting...')


def display_gallery():
    data = FakeData.gallery()
    lscat.show_instant(lscat.TrackDownloads, data, True)

def display_user():
    data = FakeData.user()
    lscat.show_instant(lscat.TrackDownloadsUsers, data)

def display_path():
    path = input('Please paste in your path:\n')
    if not Path(path).is_dir():
        print('Invalid path!')
        return

    data = FakeData(path)
    lscat.show_instant(lscat.TrackDownloads, data, True)


def browse_cache():
    path = pick_dir()
    data = FakeData(path)

    ans = input('Does this directory have a .koneko file? [y/N] ')

    if ans == 'n':
        lscat.show_instant(lscat.TrackDownloads, data, True)
    else:
        lscat.show_instant(lscat.TrackDownloadsUsers, data)

def pick_dir():
    path = KONEKODIR

    while True:
        files = sorted(os.listdir(path))
        for i, f in enumerate(files):
            print(i, '--', f)

        print('\nSelect a directory to view (enter its index)')
        print('If you want to display this directory, enter "y"')
        ans = input()
        check_quit(ans)

        if ans == 'y':
            return path

        elif ans == 'b':
            path = path.parent
            continue

        path = path / files[int(ans)]



def config_assistance():
    print(*('\n=== Configuration assistance ===',
        'Please select an action index',
        '1. Thumbnail size',
        '2. x-padding',
        '3. y-padding',
        '4. Number of columns',
        '5. Number of rows',
        '6. Page spacing',
        '7. Gallery print spacing',
        '8. User mode print info x-position',
        'a. (Run all of the above)\n'), sep='\n')
    ans = input()

    if ans in {'1', 'a'}:
        size = thumbnail_size_assistant()
    else:
        size = config.thumbnail_size_config()

    if ans in {'2', 'a'}:
        xpadding = xpadding_assistant(size)

    if ans in {'3', 'a'}:
        ypadding = ypadding_assistant(size)

    if ans in {'4', 'a'}:
        ncols = ncols_assistant(size)

    if ans in {'6', 'a'}:
        page_spacing = page_spacing_assistant(size)

    if ans in {'7', 'a'}:
        gallery_print_spacing = gallery_print_spacing_assistant()

    if ans in {'8', 'a'}:
        user_info_xcoord = user_print_name_spacing_assistant(size)


    print('\nYour recommended settings are:')
    if ans in {'1', 'a'}:
        print(f'image_thumbnail_size = {size}')

    if ans in {'2', 'a'}:
        print(f'images_x_spacing = {xpadding}')

    if ans in {'3', 'a'}:
        print(f'images_y_spacing = {ypadding}')

    if ans in {'4', 'a'}:
        print(f'number_of_columns = {ncols}')

    if ans in {'6', 'a'}:
        print(f'page_spacing = {page_spacing}')

    if ans in {'7', 'a'}:
        print(f'gallery_print_spacing =',
              ','.join((str(x) for x in gallery_print_spacing)))

    if ans in {'8', 'a'}:
        print(f'users_print_name_xcoord = {user_info_xcoord}')

    input('\nEnter any key to quit\n')


def thumbnail_size_assistant():
    """=== Thumbnail size ===
    This will display an image whose thumbnail size can be varied
    Use +/= to increase the size, and -/_ to decrease it
    Use q to exit the program, and press enter to confirm the size

    Keep in mind this size will be used for a grid of images
    """
    print_doc(thumbnail_size_assistant.__doc__)

    image = copy(SAMPLE_IMAGE)

    size = 300  # starting size
    with term.cbreak():
        while True:
            image.thumbnail(size).show(align='left', x=0, y=0)

            ans = term.inkey()
            check_quit(ans)

            if ans in PLUS:
                size += 20

            elif ans in MINUS:
                image.hide()
                size -= 20

            elif ans.code == 343:  # Enter
                return size

            #elif ans == 't':
            # TODO: preview a grid with chosen size


def xpadding_assistant(thumbnail_size):
    """=== Image x spacing ===
    1) Move the second image so that it is just to the right of the first image
       Use +/= to move it to the right, and -/_ to move it to the left.
       Press enter to confirm

    2) Based on the position of the second image, adjust its position to suit you.
       This value will be the x spacing

    Use q to exit the program, and press enter to go to the next assistant
    """
    return abstract_padding(
            thumbnail_size,
            show_single,
            config.xcoords_config()[0],
            xpadding_assistant.__doc__,
            'x',
            'width',
            False,
        )

def ypadding_assistant(thumbnail_size):
    """=== Image y spacing ===
    1) Move the second image so that it is just to the bottom of the first image
       Use +/= to move it downwards, and -/_ to move it upwards.
       Press enter to confirm

    2) Based on the current height of the second image, adjust its height to suit you.
       This value will be the y spacing

    Use q to exit the program, and press enter to go to the next assistant
    """
    return abstract_padding(
            thumbnail_size,
            show_single_y,
            config.xcoords_config()[1],
            ypadding_assistant.__doc__,
            'y',
            'height',
            True,
        )

def abstract_padding(
        thumbnail_size: int,
        show_func: 'func',
        default_x: int,
        doc: str,
        dimension: str,
        width_or_height: str,
        move: bool,
    ) -> 'IO':

    print_doc(doc)

    show_single(default_x, thumbnail_size)

    image_width, image = find_image_width(
        thumbnail_size,
        show_func,
        move,
        width_or_height
    )

    if move:
        move_cursor_down(image_width)

    spaces = 0

    with term.cbreak():
        while True:
            if move:
                move_cursor_up(spaces)
            print('\r' + ' ' * 20, end='', flush=True)
            print('\r', end='', flush=True)
            print(f'{dimension} spacing = {spaces}', end='', flush=True)

            ans = term.inkey()
            check_quit(ans)

            if ans.code == 343:  # Enter
                return spaces

            if spaces >= 0:
                image.hide()
                move_cursor_up(1)

            if ans in PLUS:
                spaces += 1

            elif ans in MINUS and spaces > 0:
                spaces -= 1

            image = show_func(image_width + spaces, thumbnail_size)


def find_image_width(thumbnail_size, show_func, move, width_or_height):
    image = None
    spaces = 0
    valid = True

    with term.cbreak():
        while True:
            if valid:
                if move:
                    move_cursor_up(spaces)
                erase_line()
                print(f'image {width_or_height} = {spaces}', end='', flush=True)

            ans = term.inkey()
            check_quit(ans)

            if ans.code == 343:  # Enter
                erase_line()
                return spaces, image

            if spaces > 0 and image:
                image.hide()
                move_cursor_up(1)

            if ans in PLUS:
                spaces += 1

            elif ans in MINUS and spaces > 0:
                spaces -= 1

            else:
                valid = False
                continue

            image = show_func(spaces, thumbnail_size)
            valid = True


def ncols_assistant(thumbnail_size):
    """=== Number of columns ===
    Use +/= to show another column, and -/_ to hide the rightmost column
    Increase the number of columns just until no more can fit in your screen

    Use q to exit the program, and press enter to go to the next assistant
    """
    print_doc(ncols_assistant.__doc__)

    xcoords = config.xcoords_config() * 2

    show_single(xcoords[0], thumbnail_size)

    images = []  # LIFO stack
    i = 0  # Zero index to make indexing `images` easier
    with term.cbreak():
        while True:
            erase_line()
            print(f'Number of columns = {i + 1}', end='', flush=True)

            ans = term.inkey()
            check_quit(ans)

            if ans in PLUS:
                images.append(show_single(xcoords[i + 1], thumbnail_size))
                i += 1

            elif ans in MINUS and images:
                i -= 1
                images[i].hide()
                images.pop(i)
                move_cursor_up(1)

            elif ans.code == 343:  # Enter
                print('')
                return i + 1  # Zero index


def page_spacing_assistant(thumbnail_size):
    # This doesn't use print_doc() as a clean state is needed
    print('\n=== Page spacing ===')
    print('This will display an image, then print newlines.')
    print('Your desired setting is the number when '
          'the image completely scrolls out of view')

    input('Enter any key to continue\n')
    os.system('clear')

    copy(SAMPLE_IMAGE).thumbnail(thumbnail_size).show(align='left')

    time.sleep(0.5)

    for i in range(term.height + 5):
        print(i)
        time.sleep(0.1)

    print('When the image just completely scrolls out of view, '
          'what is the largest number?')
    print('(By default on kitty, ctrl+shift+up/down '
          'scrolls up/down a line)')
    return input()


def gallery_print_spacing_assistant():
    print('\n=== Gallery print spacing ===')
    print('Print spacing is the number of blank spaces between each number')
    print('For example:')
    print('x' * 9, '1', 'x' * 17, '2', 'x' * 17, '3', '...', sep='')

    # TODO: print key info in selecting screen
    print('\nUse +/= to increase the spacing, and -/_ to decrease it')
    print('Use q to exit the program, and press enter to go to the next assistant\n')
    print('Use left and right arrow keys to change the current space selection')

    print('\nPick a directory to preview in grid first')

    input('\nEnter any key to continue\n')
    os.system('clear')

    path = pick_dir()
    data = FakeData(path)
    lscat.show_instant(lscat.TrackDownloads, data)
    print('\n')

    ncols = config.ncols_config()
    spacing = [9, 17, 17, 17, 17] + [17] * (ncols - 5)
    current_selection = 0

    with term.cbreak():
        while True:
            move_cursor_up(2)
            erase_line()
            print_cols(spacing, ncols)
            erase_line()
            print(f'\nAdjusting number {current_selection+1}', flush=True)

            ans = term.inkey()
            check_quit(ans)

            if ans in PLUS:
                new = int(spacing[current_selection]) + 1
                if line_width(spacing, ncols) < term.width:
                    spacing[current_selection] = new

            elif ans in MINUS:
                spacing[current_selection] = int(spacing[current_selection]) - 1
                if spacing[current_selection] < 0:
                    spacing[current_selection] = 0

            # right arrow
            elif ans.code == 261 or ans in {'d', 'l'}:
                current_selection += 1
                if current_selection >= len(spacing):
                    current_selection -= 1

            # left arrow
            elif ans.code == 260 or ans in {'a', 'h'}:
                if current_selection > 0:
                    current_selection -= 1

            elif ans.code == 343:  # Enter
                return spacing


def user_print_name_spacing_assistant(thumbnail_size):
    """=== User print name xcoord ===
    Use +/= to move the text right, and -/_ to move it left
    Adjust the position as you see fit

    Use q to exit the program, and press enter to confirm the current position
    """
    print_doc(user_print_name_spacing_assistant.__doc__)

    spacing, padding = config.get_gen_users_settings()
    preview_xcoords = config.xcoords_config(offset=1)[-3:]

    display_user_row(thumbnail_size, preview_xcoords, padding)
    move_cursor_up(5)

    with term.cbreak():
        while True:
            erase_line()         # Erase the first line
            move_cursor_down()   # Go down and erase the second line
            erase_line()
            move_cursor_up(1)    # Go back up to the original position
            print_info(spacing)  # Print info takes up 2 lines
            move_cursor_up(2)    # so go back to the top

            ans = term.inkey()
            check_quit(ans)

            if ans in PLUS:
                spacing += 1

            elif ans in MINUS:
                if spacing > 0:
                    spacing -= 1

            elif ans.code == 343:  # Enter
                print('\n' * 3)
                return spacing

