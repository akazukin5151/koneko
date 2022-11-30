"""Prompt functions that intercept keyboard input and call the right method

Structure:
    - Common prompt code
    - Core prompt loops
    - Actions dispatcher
"""

import sys

from koneko.pure import all_isdigit
from koneko import pure, utils, printer, download, TERM


# Common prompt code
def ask_quit() -> 'IO':
    """Ask for quit confirmation, no need to press enter"""
    printer.print_bottom('\nAre you sure you want to exit?', offset=1)
    with TERM.cbreak():
        ans = TERM.inkey()
        if ans == 'y' or ans == 'q' or ans.name == 'KEY_ENTER':
            sys.exit(0)


def ask_wait_user_input(
    keyseqs: 'list[str]', view_name: str, use_ueberzug=None
) -> str:
    if not keyseqs:
        printer.print_bottom(
            f'Enter {view_name} view command:', offset=-1,
            use_ueberzug=use_ueberzug
        )
    command = TERM.inkey()
    printer.print_bottom(
        command, offset=2, end='', flush=True,
        use_ueberzug=use_ueberzug
    )
    return command


def common(
    case: 'dict',
    command: str,
    keyseqs: 'list[str]',
    allowed_keys: 'tuple[str]' = tuple()
) -> 'list[str]':
    """Actions common to all prompts that do not break out of prompt.
    Returns keyseqs, modified if needed
    """
    func = case.get(command, None)
    if func:
        func()
        return keyseqs

    func = case.get(command.name, None)
    if func:
        func()
        return keyseqs

    # Wait for the rest of the sequence
    elif command.isdigit() or command in allowed_keys:
        keyseqs.append(command)
        return keyseqs

    # needs to be here not in case, because keyseqs needs to be cleared to []
    elif command.name == 'KEY_ESCAPE' or command.name == 'KEY_BACKSPACE':
        # Remove entire line
        printer.print_bottom('\r', '\b \b' * 4, end='', flush=True)
        return []

    elif not command.isdigit() or len(keyseqs) > 3:
        printer.print_bottom('\nInvalid command! Press h to show help')
        return []


# Core prompt functions
# The three prompt functions all follow the same internal structure
# inside the while loop:
# 1. Handle multi char input (either 2-digits, or one-letter-2-digits)
# 2. Ask and wait for user input (blocking)
# 3. Single char input with action that leaves the prompt
# 4. common()

def gallery_like_prompt(gallery):
    """
    Only contains logic for interpreting key presses, and do the correct action
    Sequence means a combination of more than one key.
    When a sequenceable key is pressed, wait for the next keys in the sequence
        If the sequence is valid, execute their corresponding actions
    Otherwise for keys that do not need a sequence, execute their actions normally
    """
    keyseqs = []
    sequenceable_keys = ('o', 'd', 'i', 'O', 'D', 'a', 'A')
    case = {
        'n': gallery.next_page,
        'p': gallery.previous_page,
        'h': gallery.help,
        'q': ask_quit,
        'm': lambda: printer.print_bottom('', gallery.__doc__),
        'KEY_DOWN': gallery.scroll_down,
        'KEY_UP': gallery.scroll_up,
    }

    with TERM.cbreak():
        while True:
            two_digit_seq = len(keyseqs) == 2 and all_isdigit(keyseqs)
            one_letter_two_digit_seq = (
                len(keyseqs) == 3
                and all_isdigit(keyseqs[1:])
                and keyseqs[0] in sequenceable_keys
            )

            # 1. Two digit sequence
            if two_digit_seq:
                image_num = utils.seq_coords_to_int(keyseqs)
                if image_num is not False:
                    return goto_image(gallery, image_num)
                printer.print_bottom('\nInvalid command! Press h to show help')
                keyseqs = []

            # 1. One letter two digit sequence
            elif one_letter_two_digit_seq:
                open_or_download(gallery, keyseqs)

                if keyseqs[0] == 'i':
                    return goto_image(gallery, pure.concat_seqs_to_int(keyseqs, 1))
                elif keyseqs[0].lower() == 'a':
                    return gallery.handle_prompt(keyseqs)
                keyseqs = []

            # 2. Wait for user input
            gallery_command = ask_wait_user_input(keyseqs, 'a gallery')

            # 3. Single char input with action that leaves prompt
            if gallery_command == 'b':
                return gallery.handle_prompt(['b'])

            elif gallery_command == 'r':
                return gallery.handle_prompt(['r'])

            # 4. Common
            keyseqs = common(case, gallery_command, keyseqs, sequenceable_keys)


def image_prompt(image):
    """if-else statements to intercept key presses and do the correct action"""
    keyseqs = []
    case = {
        'o': image.open_image,
        'd': image.download_image,
        'n': image.next_image,
        'p': image.previous_image,
        'f': image.show_full_res,
        'r': image.view_related_images,
        'h': printer.image_help,
        'q': ask_quit,
        'm': lambda: printer.print_bottom(image.__doc__)
    }

    with TERM.cbreak():
        while True:
            two_digit_seq = len(keyseqs) == 2 and all_isdigit(keyseqs)

            # 1. Two digit sequence -- jump to post number
            if two_digit_seq:
                image.jump_to_image(pure.concat_seqs_to_int(keyseqs))
                keyseqs = []

            # 2. Ask and wait for user input
            # image mode needs to force use_ueberzug=True, see comments in
            # lscat_prompt.ImageLoop
            image_prompt_command = ask_wait_user_input(
                keyseqs, 'an image', use_ueberzug=True
            )

            # 3. Single char input with action that leaves prompt
            if image_prompt_command == 'b':
                return image.leave(False)

            elif image_prompt_command == 'a':
                return image.leave(True)

            # 4. Common
            keyseqs = common(case, image_prompt_command, keyseqs)


def user_prompt(user):
    """Handles key presses for user views (following users and user search)
    The only difference between image and user prompts is the `case`,
    action of two-digit-sequence, and single character return
    """
    keyseqs = []
    case = {
        'n': user.next_page,
        'p': user.previous_page,
        'h': printer.user_help,
        'q': ask_quit,
        'm': lambda: printer.print_bottom(user.__class__.__bases__[0].__doc__),
        'KEY_DOWN': user.scroll_down,
        'KEY_UP': user.scroll_up,
    }

    with TERM.cbreak():
        while True:
            two_digit_seq = len(keyseqs) == 2 and all_isdigit(keyseqs)

            # 1. Two digit sequence -- view artist given number
            if two_digit_seq:
                return user.go_artist_mode(pure.concat_seqs_to_int(keyseqs))

            # 2. Ask and wait for user input
            user_prompt_command = ask_wait_user_input(keyseqs, 'a user')

            # 3. Single char input with action that leaves prompt
            if user_prompt_command == 'r':
                return user.reload()

            # 4. Common
            keyseqs = common(case, user_prompt_command, keyseqs)


# Actions dispatcher
def open_or_download(gallery, keyseqs: 'list[str]') -> 'IO':
    letter = keyseqs[0]
    first_num, second_num = keyseqs[-2:]
    if letter == 'o':
        utils.open_link_coords(gallery._data, int(first_num), int(second_num))

    elif letter == 'd':
        download.download_image_coords(gallery._data, int(first_num), int(second_num))

    selected_image_num = int(f'{first_num}{second_num}')

    if letter == 'O':
        utils.open_link_num(gallery._data, selected_image_num)

    elif letter == 'D':
        download.download_image_num(gallery._data, selected_image_num)


def goto_image(gallery, image_num: int) -> 'IO':
    if image_num is False:
        printer.print_bottom('Invalid number!')
        gallery_like_prompt(gallery)  # Go back to while loop
    else:
        gallery.view_image(image_num)
