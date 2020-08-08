"""Browse pixiv in the terminal using kitty's icat to display images (in the
terminal!)

Entry point of package, start all the while loops here and launch the
required mode.

Structure:
    - *the* main function
    - main_loop()
    - Loop
    - Frequent
"""

import os
import sys
from abc import ABC, abstractmethod

from koneko import ui, pure, utils, prompt, screens, picker, lscat_app


def main_loop(_, your_id: str) -> 'IO':
    """Ask for a mode and launch it (mode might ask for more info), for no cli args"""
    printmessage = True
    case = {
        '1': ArtistModeLoop('').start,
        '2': ViewPostModeLoop('').start,
        '3': FollowingUserModeLoop(your_id).start,
        '4': SearchUsersModeLoop('').start,
        '5': illust_follow_mode,
        '6': illust_recommended_mode,
        'f': frequent,
        '?': screens.info_screen_loop,
        'm': screens.show_man_loop,
        'b': lscat_app.browse_cache,
    }

    while True:
        main_command = screens.begin_prompt(printmessage)

        func = case.get(main_command, None)

        if main_command == '3':
            return func(your_id)

        elif func:
            return func()

        elif main_command == 'q':
            answer = input('Are you sure you want to exit? [Y/n]:\n')
            if answer == 'y' or not answer:
                sys.exit(0)
            else:
                printmessage = False
                continue

        else:
            print('\nInvalid command!')


# - Loop classes
class AbstractLoop(ABC):
    """Ask for details relevant to mode then go to mode
    prompt user for details, if no command line arguments supplied
    process input (can be overridden)
    validate input (can be overridden)
    wait for api thread to finish logging in
    activates the selected mode (needs to be overridden)
    """

    def __init__(self, user_input: 'Optional[str]'):
        self._user_input = user_input
        # Defined by classes that inherit this in _prompt_url_id()
        self._raw_answer: str
        self.mode: 'ui'

    @abstractmethod
    def _prompt_url_id(self) -> str:
        """Define self._raw_answer here"""
        raise NotImplementedError

    @abstractmethod
    def _go_to_mode(self) -> 'IO':
        """Define self.mode here"""
        raise NotImplementedError

    @abstractmethod
    def __str__(self) -> str:
        """Return a hard-coded string of the current mode number"""
        raise NotImplementedError

    def start(self) -> 'IO':
        """Ask for further info if not provided, then proceed to mode"""
        while True:
            if not self._user_input:
                self._prompt_url_id()
                self._process_raw_answer()

            if not self._validate_input():
                print('Invalid image ID!')
                self._user_input = None
                continue

            self._save_history()

            if self._user_input == '!freq':
                return frequent_modes([str(self)])
            else:
                return self._go_to_mode()

    def _process_raw_answer(self) -> str:
        """Process self._raw_answer here into self._user_input"""
        self._user_input = pure.process_user_url(self._raw_answer)

    def _validate_input(self) -> bool:
        if self._user_input == '!freq':
            return True

        try:
            int(self._user_input)
        except ValueError:
            return False
        return True

    def _save_history(self) -> 'IO':
        if self._user_input != '!freq':
            logger = utils.setup_history_log()
            logger.info(str(self) + ': ' + self._user_input)


class ArtistModeLoop(AbstractLoop):
    """Ask for artist ID and process it, then go to mode 1"""

    def _prompt_url_id(self) -> str:
        """Implements abstractmethod: prompt for artist ID or url"""
        self._raw_answer = input('Enter artist ID or url:\n')

    def _go_to_mode(self) -> 'IO':
        """Implements abstractmethod: go to mode 1"""
        os.system('clear')
        self.mode = ui.ArtistGallery(self._user_input)
        prompt.gallery_like_prompt(self.mode)

    def __str__(self) -> str:
        """Implements abstractmethod: return string of mode number"""
        return '1'


class ViewPostModeLoop(AbstractLoop):
    """Ask for post ID and process it, then go to mode 2"""

    def _prompt_url_id(self) -> str:
        """Implements abstractmethod: prompt for post url or ID"""
        self._raw_answer = input('Enter pixiv post url or ID:\n')

    def _process_raw_answer(self) -> str:
        """Overrides base method for different processor function"""
        self._user_input = pure.process_artwork_url(self._raw_answer)

    def _go_to_mode(self) -> 'IO':
        """Implements abstractmethod: Go to mode 2"""
        os.system('clear')
        ui.view_post_mode(self._user_input)

    def __str__(self) -> str:
        """Implements abstractmethod: return string of mode number"""
        return '2'


class FollowingUserModeLoop(AbstractLoop):
    """If ID given via cli, immediately go to mode 3
    Else, ask if the ID saved in the config should be used.
    If yes, prompt_url_id() will be skipped
    If not, ask for pixiv ID or url and process it.
    """

    def _prompt_url_id(self) -> str:
        """Implements abstractmethod: prompt for artist ID or url"""
        self._raw_answer = input('Enter your pixiv ID or url: ')

    def _go_to_mode(self) -> 'IO':
        """Implements abstractmethod: Go to mode 3"""
        os.system('clear')
        self.mode = ui.FollowingUsers(self._user_input)
        prompt.user_prompt(self.mode)

    def start(self, your_id=None) -> 'IO':
        """Complements base method: If ID not given, ask if config ID should be used,
        or enter a custom ID
        """
        if your_id:
            self._user_input = ask_your_id(your_id)
        super().start()

    def __str__(self) -> str:
        """Implements abstractmethod: return string of mode number"""
        return '3'


class SearchUsersModeLoop(AbstractLoop):
    """Ask for search string and go to mode 4"""

    def _prompt_url_id(self) -> str:
        """Implements abstractmethod: prompt for search string"""
        self._raw_answer = input('Enter search string:\n')

    def _process_raw_answer(self) -> str:
        """Overrides base method: nothing to process"""
        self._user_input = self._raw_answer

    def _validate_input(self) -> bool:
        """Overrides base method: all inputs are valid"""
        return True

    def _go_to_mode(self) -> 'IO':
        """Implements abstractmethod: Go to mode 4"""
        os.system('clear')
        self.mode = ui.SearchUsers(self._user_input)
        prompt.user_prompt(self.mode)

    def __str__(self) -> str:
        """Implements abstractmethod: return string of mode number"""
        return '4'


def illust_follow_mode() -> 'IO':
    """Immediately goes to ui.IllustFollowGallery()"""
    mode = ui.IllustFollowGallery()
    prompt.gallery_like_prompt(mode)


def illust_recommended_mode() -> 'IO':
    """Immediately goes to ui.IllustRecommendedGallery()"""
    mode = ui.IllustRecommendedGallery()
    prompt.gallery_like_prompt(mode)


def frequent_modes(modes: 'list[str]') -> 'IO':
    history = utils.frequent_history_modes(modes)
    actions = utils.format_frequent(history)
    _frequent(actions, history)


def frequent() -> 'IO':
    history = utils.frequent_history()
    actions = utils.format_frequent(history)
    _frequent(actions, history)


def _frequent(actions: 'list[str]', history: 'dict[str, int]') -> 'IO':
    idx = picker.frequent_modes_picker(actions)
    if idx == 'f':
        return frequent_modes(picker.select_modes_filter())

    ans = tuple(history)[idx]
    mode, user_input = ans.split(': ')

    case = {
        '1': ArtistModeLoop(user_input).start,
        '2': ViewPostModeLoop(user_input).start,
        '3': FollowingUserModeLoop(user_input).start,
        '4': SearchUsersModeLoop(user_input).start,
    }
    func = case.get(mode, None)
    if func:
        func()


def ask_your_id(your_id: str):
    if your_id:  # your_id stored in config file
        ans = input('Do you want to use the Pixiv ID saved in your config? [Y/n]\n')
        if ans in {'y', ''}:
            return your_id

    # If your_id not stored, or if ans is no, or if id provided, via cli
    return ''
