import os
import sys
from shutil import rmtree

from pick import Picker
from placeholder import m

from koneko import utils, files, screens, assistants, KONEKODIR


# Constants
EMPTY_FILTER_WARNING = "~~No directories match the filter! Press 'f' to re-filter~~"
EMPTY_WARNING = '~~This directory is empty!~~'


def ws_picker(actions: 'list[str]', title: str, **kwargs) -> Picker:
    picker = Picker(actions, title, **kwargs)
    picker.register_custom_handler(ord('w'), m.move_up())
    picker.register_custom_handler(ord('s'), m.move_down())
    return picker


def _pick_dirs_picker(actions: 'list[str]', title: str) -> Picker:
    picker = ws_picker(actions, title)
    picker.register_custom_handler(ord('y'), lambda p: (None, 'y'))
    picker.register_custom_handler(ord('b'), lambda p: (None, 'b'))
    picker.register_custom_handler(ord('f'), lambda p: (None, 'f'))
    picker.register_custom_handler(ord('d'), lambda p: (None, 'd'))
    picker.register_custom_handler(ord('c'), lambda p: (None, 'c'))
    picker.register_custom_handler(ord('q'), lambda p: (None, 'q'))
    return picker


def lscat_app_main() -> int:
    os.system('clear')
    title = ('Welcome to the lscat interactive script\n'
             'Please select an action')
    actions = (
        '1. Launch koneko configuration assistance',
        '2. Browse a cached dir to display',
        '3. Display a specified path',
        '4. Display KONEKODIR / testgallery',
        '5. Display KONEKODIR / testuser',
        'Quit'
    )
    mypicker = ws_picker(actions, title)
    _, ans = mypicker.start()
    return ans


def frequent_modes_picker(actions: 'list[str]') -> int:
    title = (
        'Please pick an input\n'
        '[mode]: [pixiv ID or searchstr] (frequency)\n'
        "Press 'f' to filter modes"
    )

    mypicker = ws_picker(actions, title)
    mypicker.register_custom_handler(ord('f'), lambda p: (None, 'f'))

    _, idx = mypicker.start()
    return idx


def _multiselect_picker(actions: 'list[str]', title: str, to_str=True) -> 'IO[list[int | str]]':
    """Returns a list of all the indices of actions"""
    picker = ws_picker(actions, title, multiselect=True, min_selection_count=1)
    selected = picker.start()
    if to_str:
        return [str(x[1] + 1) for x in selected]
    return [x[1] + 1 for x in selected]


def select_modes_filter(more=False) -> 'IO[list[str]]':
    title = 'Use SPACE to select a mode to show and ENTER to confirm'
    # Copied from screens
    actions = [
        '1. View artist illustrations',
        '2. Open pixiv post',
        '3. View following artists',
        '4. Search for artists',
    ]

    if more:
        actions.extend(('5. View illustrations of all following artists',
                        'c. Clear all filters'))

    return _multiselect_picker(actions, title, to_str=True)


def ask_assistant() -> 'IO[list[int]]':
    """Ask for which koneko config assistant (lscat app)"""
    title = ('=== Configuration assistance ===\n'
             'Press SPACE to select an action & ENTER to confirm')

    actions = (
        '1. Thumbnail size',
        '2. x-padding',
        '3. y-padding',
        '4. Page spacing',
        '5. Gallery print spacing',
        '6. User mode print info x-position',
        'a. (Run all of the above)\n',
        'Quit'
    )

    return _multiselect_picker(actions, title, to_str=False)


def pick_dir() -> 'path':
    path = KONEKODIR
    # base is immutable
    cache_size = utils.get_cache_size()
    basetitle = (
        'Select a directory to view, or press:\n'
        "y: display the current directory\n"
        "b: move up a directory\n"
        "f: filter out modes\n"
        "d: delete the current directory\n"
        "c: clear the entire cache\n"
        "q: exit\n"
        f"Current cache size: {cache_size}"
    )
    actions = files.filter_history(path)
    return _pick_dir_loop(path, basetitle, actions, [])


def _pick_dir_loop(path, basetitle, actions, modes) -> 'path':
    title = basetitle

    while True:
        picker = _pick_dirs_picker(actions, title)
        _, ans = picker.start()
        assistants.check_quit(ans)

        if ans == 'y' and files.path_valid(path):
            return path

        elif ans == 'b':
            path = handle_back(path)

        elif ans == 'd':
            path = handle_delete(path)

        elif ans == 'f':
            title, actions, modes = handle_filter(path, basetitle)
            continue

        elif ans == 'c':
            handle_clear()

        elif isinstance(ans, int):
            path, modes = handle_cd(path, actions[ans], modes)

        actions = actions_from_dir(path, modes)


def handle_back(path: 'path') -> 'path':
    if path != KONEKODIR:
        return path.parent
    return path


def handle_delete(path: 'path') -> 'IO[path]':
    print(f'Are you sure you want to delete {path}?')
    confirm = input("Enter 'y' to confirm\n")
    if confirm == 'y':
        rmtree(path)
        return path.parent
    return path


def handle_filter(path: 'path', basetitle: str) -> (str, 'list[str]', 'list[str]'):
    modes = select_modes_filter(True)
    if '6' in modes:
        # Clear all filters
        actions = files.filter_history(path)
        return basetitle, actions, modes

    title = f'Filtering {modes=}\n' + basetitle
    actions = try_filter_dir(modes)
    return title, actions, modes


def handle_cd(path, selected_dir: 'path', modes: 'list[str]') -> ('path', 'list[str]'):
    if selected_dir == EMPTY_FILTER_WARNING:
        return KONEKODIR, []
    elif (newpath := path / selected_dir).is_dir():
        return newpath, modes
    return path, modes


def actions_from_dir(path: 'path', modes: 'list[str]') -> 'list[str]':
    if path == KONEKODIR and modes:  # Filter active
        return try_filter_dir(modes)
    if ('2' in modes
            and '1' not in modes
            and 'individual' not in str(path)
            and str(path.name).isdigit()):
        return ['individual']

    return files.filter_history(path) or [EMPTY_WARNING]


def try_filter_dir(modes: 'list[str]') -> 'list[str]':
    return sorted(files.filter_dir(modes)) or [EMPTY_FILTER_WARNING]

def handle_clear():
    if screens.clear_cache_loop():
        sys.exit(0)
