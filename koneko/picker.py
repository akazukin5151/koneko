import os
from shutil import rmtree

from pick import Picker
from placeholder import m

from koneko import files, assistants, KONEKODIR


# Constants
EMPTY_WARNING = "**No directories match the filter! Press 'f' to re-filter**"


def ws_picker(actions, title, **kwargs):
    picker = Picker(actions, title, **kwargs)
    picker.register_custom_handler(ord('w'), m.move_up())
    picker.register_custom_handler(ord('s'), m.move_down())
    return picker


def _pick_dirs_picker(actions, title):
    picker = Picker(actions, title)
    picker.register_custom_handler(ord('w'), m.move_up())
    picker.register_custom_handler(ord('s'), m.move_down())
    picker.register_custom_handler(ord('y'), lambda p: (None, 'y'))
    picker.register_custom_handler(ord('b'), lambda p: (None, 'b'))
    picker.register_custom_handler(ord('f'), lambda p: (None, 'f'))
    picker.register_custom_handler(ord('d'), lambda p: (None, 'd'))
    picker.register_custom_handler(ord('q'), lambda p: (None, 'q'))
    return picker


def lscat_app_main():
    os.system('clear')
    title = ('Welcome to the lscat interactive script\n'
             'Please select an action')
    actions = (
        '1. Launch koneko configuration assistance',
        '2. Display KONEKODIR / testgallery',
        '3. Display KONEKODIR / testuser',
        '4. Browse a cached dir to display',
        '5. Display a specified path',
        'Quit'
    )
    mypicker = ws_picker(actions, title)
    _, ans = mypicker.start()
    return ans


def frequent_modes_picker(actions):
    title = (
        "Please pick an input\n"
        "[mode]: [pixiv ID or searchstr] (frequency)\n"
        "Press 'f' to filter modes"
    )

    mypicker = ws_picker(actions, title)
    mypicker.register_custom_handler(ord('f'), lambda p: (None, 'f'))

    _, idx = mypicker.start()
    return idx


def _multiselect_picker(actions, title, to_str=True) -> 'IO[list[int]]':
    """Returns a list of all the indices of actions"""
    picker = ws_picker(actions, title, multiselect=True, min_selection_count=1)
    selected = picker.start()
    if to_str:
        return [str(x[1] + 1) for x in selected]
    return [x[1] + 1 for x in selected]


def select_modes_filter(more=False):
    title = "Use SPACE to select a mode to show and ENTER to confirm"
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


def pick_dir():
    path = KONEKODIR
    # base is immutable
    basetitle = (
        'Select a directory to view\n'
        "Press 'y' to display the current directory\n"
        "Press 'b' to move up a directory\n"
        "Press 'd' to delete the current directory\n"
        "Press 'f' to filter out modes\n"
        "Press 'q' to exit"
    )
    actions = files.filter_history(path)
    return _pick_dir_loop(path, basetitle, actions, None)


def _pick_dir_loop(path, basetitle, actions, modes):
    title = basetitle

    while True:
        picker = _pick_dirs_picker(actions, title)
        _, ans = picker.start()
        assistants.check_quit(ans)

        if ans == 'y':
            return path  # TODO: prevent return if path is not valid

        elif ans == 'b':
            path = handle_back(path)

        elif ans == 'd':
            path = handle_delete(path)

        elif ans == 'f':
            title, actions, modes = handle_filter(path, basetitle)
            continue

        else:
            path, modes = handle_cd(path, actions, ans, modes)

        actions = actions_from_dir(path, modes)


def handle_back(path):
    if path != KONEKODIR:
        return path.parent
    return path


def handle_delete(path):
    print(f'Are you sure you want to delete {path}?')
    confirm = input("Enter 'y' to confirm\n")
    if confirm == 'y':
        rmtree(path)
        return path.parent
    return path


def handle_filter(path, basetitle):
    modes = select_modes_filter(True)
    if '6' in modes:
        # Clear all filters
        actions = files.filter_history(path)
        return basetitle, actions, modes

    title = f"Filtering {modes=}\n" + basetitle
    actions = try_filter_dir(modes)
    return title, actions, modes


def handle_cd(path, actions, ans, modes):
    selected_dir = actions[ans]
    if selected_dir == EMPTY_WARNING:
        return KONEKODIR, None
    elif (newpath := path / selected_dir).is_dir():
        return newpath, modes
    return path, modes


def actions_from_dir(path, modes):
    if path == KONEKODIR and modes is not None:  # Filter active
        return try_filter_dir(modes)
    return files.filter_history(path)


def try_filter_dir(modes):
    return sorted(files.filter_dir(modes)) or [EMPTY_WARNING]
