"""Prompt functions that intercept keyboard input and call the right method"""

import sys
import time

from blessed import Terminal

from koneko import ui, pure, utils, colors, download

TERM = Terminal()

def ask_quit():
    """Ask for quit confirmation, no need to press enter"""
    print('\nAre you sure you want to exit?')
    with TERM.cbreak():
        while True:
            ans = TERM.inkey()
            if ans == 'y' or ans == 'q' or ans.code == 343:  # Enter
                sys.exit(0)
            elif ans:
                break


def open_or_download(gallery, keyseqs: 'list[str]'):
    first_num, second_num = keyseqs[-2:]
    if keyseqs[0] == 'o':
        utils.open_link_coords(gallery.data, int(first_num), int(second_num))

    elif keyseqs[0] == 'd':
        download.download_image_coords(gallery.data, int(first_num), int(second_num))

    selected_image_num = int(f'{first_num}{second_num}')

    if keyseqs[0] == 'O':
        utils.open_link_num(gallery.data, selected_image_num)

    elif keyseqs[0] == 'D':
        download.download_image_num(gallery.data, selected_image_num)

def goto_image(gallery, image_num: int):
    if image_num is False:
        print('Invalid number!')
        gallery_like_prompt(gallery)  # Go back to while loop
    else:
        gallery.view_image(image_num)

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

    with TERM.cbreak():
        while True:
            gallery_command = TERM.inkey()
            print(gallery_command, end='', flush=True)

            if gallery_command.isdigit() or gallery_command in sequenceable_keys:
                keyseqs.append(gallery_command)

            # Single char input
            if gallery_command == 'n':
                gallery.next_page()

            elif gallery_command == 'p':
                ui.previous_page(gallery.data)

            elif gallery_command == 'm':
                print('')
                print(gallery.__doc__)

            elif gallery_command == 'h':
                print('')
                gallery.help()

            elif gallery_command.code == 361:  # Escape
                keyseqs = []
                # Remove entire line
                print("\r", "\b \b" * 4, end='', flush=True)

            elif gallery_command == 'b':
                return gallery.handle_prompt(['b'])

            elif gallery_command == 'r':
                return gallery.handle_prompt(['r'])

            elif gallery_command == 'q':
                ask_quit()
                print("Enter a gallery command:")

            # Multi char sequence
            if len(keyseqs) == 2 and keyseqs[0].isdigit() and keyseqs[1].isdigit():
                return goto_image(gallery, utils.seq_to_num(keyseqs))

            elif (len(keyseqs) == 3 and keyseqs[1].isdigit() and keyseqs[2].isdigit()
                  and keyseqs[0] in sequenceable_keys):

                open_or_download(gallery, keyseqs)

                if keyseqs[0] == 'i':
                    return goto_image(gallery, utils.seq_to_int(keyseqs, 1))
                elif keyseqs[0].lower() == 'a':
                    return gallery.handle_prompt(keyseqs)
                else:
                    keyseqs = []

            if len(keyseqs) > 3:
                print('\nInvalid command! Press h to show help')
                keyseqs = []


def image_prompt(image):
    """
    if-else statements to intercept key presses and do the correct action
    current_page and current_page_num is for gallery view -> next page(s) ->
    image prompt -> back
    kwargs are to store info for posts with multiple pages/images
    """
    case = {
        'o': image.open_image,
        'd': image.download_image,
        'n': image.next_image,
        'p': image.previous_image,
        'f': image.show_full_res,
    }

    keyseqs = []
    with TERM.cbreak():
        while True:
            print('Enter an image view command:')
            image_prompt_command = TERM.inkey()

            # Simplify if-else chain with case-switch
            func = case.get(image_prompt_command, None)
            if func:
                func()

            elif image_prompt_command.isdigit():
                keyseqs.append(image_prompt_command)
                print(keyseqs)

            elif image_prompt_command.code == 361:  # Escape
                keyseqs = []
                print(keyseqs)

            elif image_prompt_command == 'm':
                print(image.__doc__)

            elif image_prompt_command == 'h':
                print(''.join([
                    colors.b, 'ack; ',
                    colors.n, 'ext image; ',
                    colors.p, 'revious image; ',
                    colors.d_, 'ownload image;',
                    colors.o_, 'pen image in browser;\n',
                    'show image in', colors.f, 'ull res; ',
                    colors.q, 'uit (with confirmation); ',
                    'view ', colors.m, 'anual\n'
                ]))

            elif image_prompt_command == 'q':
                ask_quit()

            elif image_prompt_command == 'b':
                return image.leave(False)

            elif image_prompt_command == 'a':
                return image.leave(True)

            elif image_prompt_command:
                print('Invalid command! Press h to show help')

            # Two digit sequence -- jump to post number
            if len(keyseqs) == 2 and keyseqs[0].isdigit() and keyseqs[1].isdigit():
                ui.jump_to_image(image.data, utils.seq_to_int(keyseqs))
                keyseqs = []

def user_prompt(user):
    """
    Handles key presses for user views (following users and user search)
    """
    keyseqs = []
    with TERM.cbreak():
        while True:
            print('Enter a user view command:')
            user_prompt_command = TERM.inkey()

            if user_prompt_command == 'n':
                user.next_page()
                # Prevents catching "n" and messing up the cache
                time.sleep(0.5)

            elif user_prompt_command == 'p':
                ui.previous_page_users(user.data)

            elif user_prompt_command == 'r':
                return user.reload()

            # Wait for the rest of the sequence
            elif user_prompt_command.isdigit():
                keyseqs.append(user_prompt_command)
                print(keyseqs)

            elif user_prompt_command == 'q':
                ask_quit()

            elif user_prompt_command == 'm':
                print(ui.Users.__doc__)

            elif user_prompt_command == 'h':
                print(''.join([
                    "view ", colors.BLUE_N, "th artist's illusts ",
                    colors.n, 'ext page; ',
                    colors.p, 'revious page; ',
                    colors.r, 'eload and re-download all;\n',
                    colors.q, 'uit (with confirmation);',
                    'view ', colors.m, 'anual\n'
                ]))

            elif user_prompt_command:
                print('Invalid command! Press h to show help')
                keyseqs = []

            # End of the sequence...
            # Two digit sequence -- view artist given number
            if len(keyseqs) == 2 and keyseqs[0].isdigit() and keyseqs[1].isdigit():
                return user.go_artist_mode(utils.seq_to_int(keyseqs))
