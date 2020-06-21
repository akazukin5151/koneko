"""Browse pixiv in the terminal using kitty's icat to display images (in the
terminal!)

Entry point of package, start all the while loops here and launch the
required mode.

Capitalized tag definitions:
    TODO: to-do, high priority
    SPEED: speed things up, high priority
    FEATURE: extra feature, low priority
    BLOCKING: this is blocking the prompt but I'm stuck on how to proceed
"""

import os
import sys
from abc import ABC, abstractmethod
from pathlib import Path

from koneko import ui, api, cli, pure, config, prompt, screens


def handle_missing_pics() -> 'IO':
    print('Please wait, downloading welcome image (this will only occur once)...')
    baseurl = 'https://raw.githubusercontent.com/twenty5151/koneko/master/pics/'
    basedir = Path('~/.local/share/koneko/pics').expanduser()

    basedir.mkdir(parents=True)
    for pic in ('71471144_p0.png', '79494300_p0.png'):
        os.system(f'curl -s {baseurl}{pic} -o {basedir}{pic}')

    os.system('clear')

def handle_cli() -> (bool, str, str):
    # no cli arguments, prompt user for mode selection
    if len(sys.argv) <= 1:
        return True, '', ''

    main_command, user_input = cli.process_cli_args()
    if main_command == 'vh':
        sys.exit(0)
    return False, main_command, user_input

def main():
    """Read config file, start login, process any cli arguments, go to main loop"""
    prompted, main_command, user_input = handle_cli()

    os.system('clear')
    credentials, your_id = config.begin_config()

    if not Path('~/.local/share/koneko').expanduser().exists():
        handle_missing_pics()

    api.myapi.credentials = credentials
    api.myapi.start()
    # After this part, the API is logging in in the background and we can proceed

    try:
        main_loop(prompted, main_command, user_input, your_id)
    except KeyboardInterrupt:
        # If ctrl+c pressed before a mode is selected, thread will never join
        # Get it to join first so that modes still work
        api.myapi.await_login()
        main()

def main_loop(prompted: bool, main_command, user_input, your_id: str):
    """
    Ask for mode selection, if no command line arguments supplied
    call the right function depending on the mode
    user_input : str or int
        For artist_illusts_mode, it is artist_user_id : int
        For view_post_mode, it is image_id : int
        For following users mode, it is your_id : int
        For search users mode, it is search_string : str
        For illust following mode, it's not required
    """
    printmessage = True
    case = {
        '1': ArtistModeLoop(prompted, user_input).start,
        '2': ViewPostModeLoop(prompted, user_input).start,
        '4': SearchUsersModeLoop(prompted, user_input).start,
        '5': illust_follow_mode_loop,
        '?': screens.info_screen_loop,
        'm': screens.show_man_loop,
        'c': screens.clear_cache_loop,
    }

    while True:
        if prompted and not user_input:
            main_command = screens.begin_prompt(printmessage)

        # Simplify if-else chain with case-switch
        func = case.get(main_command, None)
        if func:
            func()

        elif main_command == '3':
            if your_id and not user_input:  # your_id stored in config file
                ans = input('Do you want to use the Pixiv ID saved in your config?\n')
                if ans in {'y', ''}:
                    FollowingUserModeLoop(prompted, your_id).start()

            # If your_id not stored, or if ans is no, or if id provided, via cli
            FollowingUserModeLoop(prompted, user_input).start()

        elif main_command == 'q':
            answer = input('Are you sure you want to exit? [Y/n]:\n')
            if answer == 'y' or not answer:
                sys.exit(0)
            else:
                printmessage = False
                continue

        else:
            print('\nInvalid command!')



#- Loop classes
class AbstractLoop(ABC):
    """Ask for details relevant to mode then go to mode
    prompt user for details, if no command line arguments supplied
    process input (can be overridden)
    validate input (can be overridden)
    wait for api thread to finish logging in
    activates the selected mode (needs to be overridden)
    """
    def __init__(self, prompted: bool, user_input: str):
        self._prompted = prompted
        self._user_input = user_input
        # Defined by classes that inherit this in _prompt_url_id()
        self._url_or_id: str
        self.mode: 'ui'

    def start(self):
        """Ask for further info if not provided, then proceed to mode"""
        while True:
            if self._prompted and not self._user_input:
                self._prompt_url_id()
                self._process_url_or_input()
                if not self._validate_input():
                    return False
                os.system('clear')

            self._go_to_mode()

    @abstractmethod
    def _prompt_url_id(self) -> str:
        """define self._url_or_id here"""
        raise NotImplementedError

    def _process_url_or_input(self) -> str:
        self._user_input = pure.process_user_url(self._url_or_id)

    def _validate_input(self) -> 'Maybe[int]':
        try:
            return int(self._user_input)
        except ValueError:
            print('Invalid image ID!')
            return False

    @abstractmethod
    def _go_to_mode(self):
        """Define self.mode here"""
        raise NotImplementedError


class ArtistModeLoop(AbstractLoop):
    """
    Ask for artist ID and process it, wait for API to finish logging in
    before proceeding
    """
    def _prompt_url_id(self) -> str:
        self._url_or_id = input('Enter artist ID or url:\n')

    def _go_to_mode(self):
        #self.mode = ui.ArtistGallery(self._user_input)
        self.mode = ui.ArtistGallery(self._user_input)
        prompt.gallery_like_prompt(self.mode)
        # This is the entry mode, user goes back but there is nothing to catch it
        main()


class ViewPostModeLoop(AbstractLoop):
    """
    Ask for post ID and process it, wait for API to finish logging in
    before proceeding
    """
    def _prompt_url_id(self) -> str:
        self._url_or_id = input('Enter pixiv post url or ID:\n')

    def _process_url_or_input(self) -> str:
        """Overriding base class to account for 'illust_id' cases"""
        self._user_input = pure.process_artwork_url(self._url_or_id)

    def _go_to_mode(self):
        ui.view_post_mode(self._user_input)
        # After backing
        main()


class SearchUsersModeLoop(AbstractLoop):
    """
    Ask for search string and process it, wait for API to finish logging in
    before proceeding
    """
    def _prompt_url_id(self) -> str:
        self._url_or_id = input('Enter search string:\n')

    def _process_url_or_input(self) -> str:
        """the 'url or id' name doesn't really apply; accepts all strings"""
        self._user_input = self._url_or_id

    def _validate_input(self) -> bool:
        """Overriding base class: all inputs are valid"""
        return True

    def _go_to_mode(self):
        self.mode = ui.SearchUsers(self._user_input)
        prompt.user_prompt(self.mode)
        main()


class FollowingUserModeLoop(AbstractLoop):
    """
    Ask for pixiv ID or url and process it, wait for API to finish logging in
    before proceeding
    If user agrees to use the your_id saved in config, prompt_url_id() will be
    skipped
    """
    def _prompt_url_id(self) -> str:
        self._url_or_id = input('Enter your pixiv ID or url: ')

    def _go_to_mode(self):
        self.mode = ui.FollowingUsers(self._user_input)
        prompt.user_prompt(self.mode)
        main()

def illust_follow_mode_loop():
    """Immediately goes to IllustFollow()"""
    while True:
        mode = ui.IllustFollowGallery()
        prompt.gallery_like_prompt(mode)
        # After backing
        main()

if __name__ == '__main__':
    main()
