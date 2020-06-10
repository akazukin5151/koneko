import os
import shutil
from subprocess import check_output

import pixcat

from koneko import KONEKODIR, ui, cli, pure, __version__


def begin_prompt(printmessage=True):
    messages = (
        '',
        f'Welcome to koneko v{__version__}\n',
        'Select an action:',
        '1. View artist illustrations',
        '2. Open pixiv post',
        '3. View following artists',
        '4. Search for artists',
        '5. View illustrations of all following artists\n',
        '?. Info',
        'm. Manual',
        'c. Clear koneko cache',
        'q. Quit',
    )
    if printmessage:
        for message in messages:
            print(' ' * 27, message)

    pixcat.Image(
        KONEKODIR.parent / 'pics' / '71471144_p0.png'
    ).thumbnail(550).show(
        align='left', y=0
    )

    cache_size = check_output(
        f"du -hs --apparent-size {KONEKODIR} | cut -f1",
        shell=True
    ).decode('utf-8').rstrip()
    print(f'Current cache size: {cache_size}')

    command = input('Enter a command: ')
    return command

@pure.catch_ctrl_c
def show_man_loop():
    os.system('clear')
    print(cli.__doc__)
    print(' ' * 3, '=' * 30)
    print(ui.ArtistGallery.__doc__)
    print(' ' * 3, '=' * 30)
    print(ui.Image.__doc__)
    print(' ' * 3, '=' * 30)
    print(ui.AbstractUsers.__doc__)
    print(' ' * 3, '=' * 30)
    print(ui.IllustFollowGallery.__doc__)
    while True:
        help_command = input('\n\nEnter any key to return: ')
        if help_command or help_command == '':
            os.system('clear')
            break

@pure.catch_ctrl_c
def clear_cache_loop():
    print('Do you want to remove all cached images?')
    print('This will not remove images you explicitly downloaded to ~/Downloads.')
    print(f'Directory to be deleted: {KONEKODIR}')
    while True:
        help_command = input('\nEnter y to confirm: ')
        if help_command == 'y':
            shutil.rmtree(KONEKODIR)
            os.system('clear')
            break
        else:
            print('Operation aborted!')
            os.system('clear')
            break

@pure.catch_ctrl_c
def info_screen_loop():
    os.system('clear')
    messages = (
        '',
        f'koneko こねこ version {__version__} beta\n',
        "Browse pixiv in the terminal using kitty's icat to display",
        'images with images embedded in the terminal\n',
        "1. View an artist's illustrations",
        '2. View a post (support multiple images)',
        '3. View artists you followed',
        '4. Search for artists and browse their works.',
        '5. View latest illustrations from artist you follow.\n',
        'Thank you for using koneko!',
        'Please star, report bugs and contribute in:',
        'https://github.com/twenty5151/koneko',
        'GPLv3 licensed\n',
        'Credits to amasyrup (甘城なつき):',
        'Welcome image: https://www.pixiv.net/en/artworks/71471144',
        'Current image: https://www.pixiv.net/en/artworks/79494300',
    )

    for message in messages:
        print(' ' * 26, message)

    pixcat.Image(
        KONEKODIR.parent / 'pics' / '79494300_p0.png'
    ).thumbnail(750).show(
        align='left', y=0
    )

    while True:
        help_command = input('\nEnter any key to return: ')
        if help_command or help_command == '':
            os.system('clear')
            break
