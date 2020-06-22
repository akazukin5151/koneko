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
    print('Welcome to the lscat interactive script')
    print('1. Launch koneko configuration assistance')
    print('2. Display KONEKODIR / testgallery')
    print('3. Display KONEKODIR / testuser')
    print('4. Display a specified path')
    ans = input('\nPlease select an action:\n')

    case = {
        '1': config_assistance,
        '2': display_gallery,
        '3': display_user,
        '4': display_path
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


def config_assistance():
    term = Terminal()

    size = thumbnail_size(term)
    gallery_spacing = gallery_page_spacing(term, size)

def thumbnail_size(term):
    print('=== Thumbnail size ===')
    print('This will display an image whose thumbnail size can be varied')
    print('Use +/= to increase the size, and -/_ to decrease it')
    print('Use q to exit the program, and press enter to confirm the size')

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


def gallery_page_spacing(term, thumbnail_size):
    print('=== Gallery page spacing ===')
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
          'what is the lowest number?')
    print('(By default on kitty, ctrl+shift+up/down '
          'scrolls up/down a line)')
    return input()


if __name__ == '__main__':
    main()
