import os
import sys
import time
from pathlib import Path

from pixcat import Image
from blessed import Terminal

from koneko import KONEKODIR, lscat, config


class FakeData:
    def __init__(self, path):
        self.download_path = path

    @classmethod
    def gallery(cls):
        return cls(KONEKODIR / 'testgallery')

    @classmethod
    def user(cls):
        # Make sure it has a .koneko file
        return cls(KONEKODIR / 'testuser')


def main():
    os.system('clear')
    print('Welcome to the lscat interactive script')
    print('1. Launch koneko configuration assistance')
    print('2. Display KONEKODIR / testgallery')
    print('3. Display KONEKODIR / testuser')
    print('4. Browse a cached dir to display')
    print('5. Display a specified path')
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

    ans = input('Should this directory be a grid (gallery), or users? [Y/n] ')

    if ans == 'n':
        lscat.show_instant(lscat.TrackDownloadsUsers, data)
    else:
        lscat.show_instant(lscat.TrackDownloads, data, True)

def pick_dir():
    path = KONEKODIR

    while True:
        files = sorted(os.listdir(path))
        for i, f in enumerate(files):
            print(i, '--', f)

        print('\nSelect a directory to view (enter its index)')
        print('If you want to display this directory, enter "y"')
        ans = input()

        if ans == 'q':
            sys.exit(0)

        elif ans == 'y':
            return path

        elif ans == 'b':
            path = path.parent
            continue

        path = path / files[int(ans)]



def config_assistance():
    term = Terminal()

    print('=== Configuration assistance ===')
    print('1. Thumbnail size')
    print('2. Page spacing')
    print('3. Gallery print spacing')
    print('4. User mode print info x-position')
    print('Please select an action index')
    print('Or enter "a" to use all')
    ans = input()

    if ans in {'1', 'a'}:
        size = thumbnail_size_assistant(term)
    else:
        size = config.thumbnail_size_config()

    if ans in {'2', 'a'}:
        page_spacing = page_spacing_assistant(term, size)

    if ans in {'3', 'a'}:
        gallery_print_spacing = gallery_print_spacing_assistant(term)

    if ans in {'4', 'a'}:
        user_info_xcoord = user_print_name_spacing_assistant(term, size)


    print('\nYour recommended settings are:')
    if ans in {'1', 'a'}:
        print(f'image_thumbnail_size = {size}')
    if ans in {'2', 'a'}:
        print(f'page_spacing = {page_spacing}')
    if ans in {'3', 'a'}:
        print(f'gallery_print_spacing =',
              ','.join((str(x) for x in gallery_print_spacing)))
    if ans in {'4', 'a'}:
        print(f'users_print_name_xcoord = {user_info_xcoord}')

    input('\nEnter any key to quit\n')

def thumbnail_size_assistant(term):
    print('=== Thumbnail size ===')
    print('This will display an image whose thumbnail size can be varied')
    print('Use +/= to increase the size, and -/_ to decrease it')
    print('Use q to exit the program, and press enter to confirm the size')

    print('\nKeep in mind this size will be used for a grid of images')

    input('Enter any key to continue\n')
    os.system('clear')
    size = 300
    image = Image(
        KONEKODIR.parent / 'pics' / '71471144_p0.png'
    )

    while True:
        with term.cbreak():
            image.thumbnail(size).show(align='left', x=0, y=0)

            ans = term.inkey()

            if ans in {'+', '='}:
                size += 20

            elif ans in {'-', '_'}:
                image.hide()
                size -= 20

            elif ans == 'q':
                sys.exit(0)

            elif ans.code == 343:  # Enter
                return size

            #elif ans == 't':
            # TODO: preview a grid with chosen size


def page_spacing_assistant(term, thumbnail_size):
    print('=== Page spacing ===')
    print('This will display an image, then print newlines.')
    print('Your desired setting is the number when '
          'the image completely scrolls out of view')

    input('Enter any key to continue\n')
    os.system('clear')

    Image(
        KONEKODIR.parent / 'pics' / '71471144_p0.png'
    ).thumbnail(thumbnail_size).show(align='left')

    time.sleep(0.5)

    for i in range(term.height + 5):
        print(i)
        time.sleep(0.1)

    print('When the image just completely scrolls out of view, '
          'what is the largest number?')
    print('(By default on kitty, ctrl+shift+up/down '
          'scrolls up/down a line)')
    return input()


def gallery_print_spacing_assistant(term):
    print('=== Gallery print spacing ===')
    print('Print spacing is the number of blank spaces between each number')
    print('For example:')
    print('x' * 9, '1', 'x' * 17, '2', 'x' * 17, '3', '...', sep='')

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

    while True:
        with term.cbreak():
            move_cursor_up(2)
            erase_line()
            print_cols(spacing, ncols)
            erase_line()
            print(f'\nAdjusting number {current_selection+1}', flush=True)

            ans = term.inkey()

            if ans in {'+', '='}:
                new = int(spacing[current_selection]) + 1
                if line_width(spacing, ncols) < term.width:
                    spacing[current_selection] = new

            elif ans in {'-', '_'}:
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

            elif ans == 'q':
                sys.exit(0)

            elif ans.code == 343:  # Enter
                return spacing


def move_cursor_up(num):
    print(f'\033[{num}A', end='', flush=True)

def move_cursor_down():
    print('\033[1B', end='', flush=True)

def erase_line():
    print('\033[K', end='', flush=True)

def print_cols(spacing, ncols):
    for (idx, space) in enumerate(spacing[:ncols]):
        print(' ' * int(space), end='', flush=True)
        print(idx + 1, end='', flush=True)

def line_width(spacing, ncols):
    return sum(spacing) + ncols


def user_print_name_spacing_assistant(term, thumbnail_size):
    print('=== User print name xcoord ===')
    print('This will display an image, then print a sample index and artist name.')
    print('\nUse +/= to move the text right, and -/_ to move it left')
    print('Adjust the position as you see fit')
    print('Use q to exit the program, and press enter to confirm the current position')

    input('\nEnter any key to continue\n')
    os.system('clear')

    spacing, padding = config.get_gen_users_settings()
    preview_xcoords = config.xcoords_config(offset=1)[-3:]

    display_user_row(thumbnail_size, preview_xcoords, padding)
    move_cursor_up(5)

    while True:
        with term.cbreak():
            erase_line()         # Erase the first line
            move_cursor_down()   # Go down and erase the second line
            erase_line()
            move_cursor_up(1)    # Go back up to the original position
            print_info(spacing)  # Print info takes up 2 lines
            move_cursor_up(2)    # so go back to the top

            ans = term.inkey()

            if ans in {'+', '='}:
                spacing += 1

            elif ans in {'-', '_'}:
                if spacing > 0:
                    spacing -= 1

            elif ans == 'q':
                sys.exit(0)

            elif ans.code == 343:  # Enter
                print('\n' * 3)
                return spacing


def display_user_row(thumbnail_size, preview_xcoords, padding):
    Image(
        KONEKODIR.parent / 'pics' / '71471144_p0.png'
    ).thumbnail(thumbnail_size).show(align='left', x=padding)

    for px in preview_xcoords:
        Image(
            KONEKODIR.parent / 'pics' / '71471144_p0.png'
        ).thumbnail(thumbnail_size).show(align='left', x=px, y=0)


def print_info(message_xcoord):
    print(' ' * message_xcoord, '000', '\n',
          ' ' * message_xcoord, 'Example artist', sep='')


if __name__ == '__main__':
    main()
