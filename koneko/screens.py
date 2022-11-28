import os
import shutil

from koneko import ui, cli, utils, config, lscat, KONEKODIR, __version__, WELCOME_IMAGE


def begin_prompt(printmessage=True) -> 'IO[str]':
    messages = (
        '',
        f'Welcome to koneko v{__version__}\n',
        'Select an action:',
        '1. View artist illustrations',
        '2. Open pixiv post',
        '3. View following artists',
        '4. Search for artists',
        '5. View illustrations of all following artists',
        '6. View recommended illustrations',
        'f. Frequent modes and user inputs',
        '',
        '?. Info',
        'm. Manual',
        'b. Browse cache (offline)',
        'q. Quit',
    )
    if printmessage:
        for message in messages:
            print(' ' * config.api.spaces_to_offset(), message)

    image = lscat.api.show(WELCOME_IMAGE, 0, 0, config.api.image_size())

    command = input('\n\nEnter a command: ')
    lscat.api.hide(image)
    return command


@utils.catch_ctrl_c
def show_man_loop() -> 'IO':
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
    input('\n\nEnter any key to return: ')
    os.system('clear')


@utils.catch_ctrl_c
def clear_cache_loop() -> 'IO[bool]':
    print('Do you want to remove all cached images?')
    print('This will not remove images you explicitly downloaded to ~/Downloads.')
    print(f'Directory to be deleted: {KONEKODIR}')
    while True:
        help_command = input('\nEnter y to confirm: ')
        if help_command == 'y':
            shutil.rmtree(KONEKODIR)
            os.system('clear')
            return True
        else:
            print('Operation aborted!')
            os.system('clear')
            return False


@utils.catch_ctrl_c
def info_screen_loop() -> 'IO':
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
        'https://github.com/akazukin5151/koneko',
        'GPLv3 licensed\n',
        'Credits to amasyrup (甘城なつき):',
        'Welcome image: https://www.pixiv.net/en/artworks/71471144',
        'Current image: https://www.pixiv.net/en/artworks/79494300',
    )

    for message in messages:
        print(' ' * 26, message)

    size = 500 if config.api.use_ueberzug() else 750
    image = lscat.api.show(WELCOME_IMAGE.parent / '79494300_p0.png', 0, 0, size)

    input('\nEnter any key to return: ')
    os.system('clear')
    lscat.api.hide(image)
