"""Prompt functions that intercept keyboard input and call the right method"""

import sys
import time

from blessed import Terminal

from koneko import main, pure, colors

TERM = Terminal()

def ask_quit():
    """Ask for quit confirmation, no need to press enter"""
    with TERM.cbreak():
        while True:
            ans = TERM.inkey()
            if ans == 'y' or ans == 'q' or ans.code == 343:  # Enter
                sys.exit(0)
            elif ans:
                break

def gallery_like_prompt(gallery_like_class):
    """
    Only contains logic for interpreting key presses, and do the correct action
    Sequence means a combination of more than one key.
    When a sequenceable key is pressed, wait for the next keys in the sequence
        If the sequence is valid, execute their corresponding actions
    Otherwise for keys that do not need a sequence, execute their actions normally
    """
    sequenceable_keys = ('o', 'd', 'i', 'O', 'D', 'a', 'A')
    with TERM.cbreak():
        keyseqs = []
        seq_num = 0
        selected_image_num, first_num, second_num = None, None, None

        print('Enter a gallery command:')
        while True:
            gallery_command = TERM.inkey()

            # Wait for the rest of the sequence
            if gallery_command in sequenceable_keys:
                keyseqs.append(gallery_command)
                print(keyseqs)
                seq_num += 1

            elif gallery_command.code == 361:  # Escape
                keyseqs = []
                seq_num = 0
                print(keyseqs)

            # Digits continue the sequence
            elif gallery_command.isdigit():
                keyseqs.append(gallery_command)
                print(keyseqs)

                # End of the sequence...
                # Two digit sequence -- view image given coords
                if seq_num == 1 and keyseqs[0].isdigit() and keyseqs[1].isdigit():

                    first_num = int(keyseqs[0])
                    second_num = int(keyseqs[1])
                    selected_image_num = pure.find_number_map(first_num, second_num)

                    break  # leave cbreak(), go to image prompt

                # One letter two digit sequence
                elif seq_num == 2 and keyseqs[1].isdigit() and keyseqs[2].isdigit():

                    first_num = keyseqs[1]
                    second_num = keyseqs[2]

                    # Open or download given coords
                    if keyseqs[0] == 'o':
                        gallery_like_class.open_link_coords(first_num, second_num)

                    elif keyseqs[0] == 'd':
                        gallery_like_class.download_image_coords(first_num, second_num)
                    elif keyseqs[0] == 'a':
                        break

                    # Open, download, or view image, given image number
                    selected_image_num = int(f'{first_num}{second_num}')

                    if keyseqs[0] == 'O':
                        gallery_like_class.open_link_num(selected_image_num)
                    elif keyseqs[0] == 'D':
                        gallery_like_class.download_image_num(selected_image_num)
                    elif keyseqs[0] == 'A':
                        break
                    elif keyseqs[0] == 'i':
                        break  # leave cbreak(), go to image prompt

                    # Reset sequence info after running everything
                    keyseqs = []
                    seq_num = 0

                # Not the end of the sequence yet, continue while block
                else:
                    seq_num += 1

            # No sequence, execute their functions immediately
            elif gallery_command == 'n':
                gallery_like_class.next_page()

            elif gallery_command == 'p':
                gallery_like_class.previous_page()

            elif gallery_command == 'q':
                print('Are you sure you want to exit?')
                ask_quit()
                # If exit cancelled
                print('Enter a gallery command:')

            elif gallery_command == 'b':
                break

            elif gallery_command == 'r':
                break

            elif gallery_command == 'm':
                print(gallery_like_class.__doc__)

            elif gallery_command == 'h':
                gallery_like_class.help()

            elif gallery_command.code == 343:  # Enter
                pass
            elif gallery_command:
                print('Invalid command! Press h to show help')
                keyseqs = []
                seq_num = 0
            # End if
        # End while
    # End cbreak()
    gallery_like_class.handle_prompt(keyseqs, gallery_command, selected_image_num,
                                     first_num, second_num)

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

    with TERM.cbreak():
        while True:
            print('Enter an image view command:')
            image_prompt_command = TERM.inkey()

            # Simplify if-else chain with case-switch
            func = case.get(image_prompt_command, None)
            if func:
                func()

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
                print('Are you sure you want to exit?')
                ask_quit()

            elif image_prompt_command == 'b':
                force = False
                break  # Leave cbreak()

            elif image_prompt_command == 'a':
                force = True
                break  # Leave cbreak()

            elif image_prompt_command == '':
                pass

            elif image_prompt_command:
                print('Invalid command! Press h to show help')

            # End if
        # End while
    # End cbreak()

    # image_prompt_command == "b"
    image.leave(force)

def user_prompt(user_class):
    """
    Handles key presses for user views (following users and user search)
    """
    keyseqs = []
    seq_num = 0
    with TERM.cbreak():
        while True:
            print('Enter a user view command:')
            user_prompt_command = TERM.inkey()

            if user_prompt_command == 'n':
                user_class.next_page()
                # Prevents catching "n" and messing up the cache
                time.sleep(0.5)

            elif user_prompt_command == 'p':
                user_class.previous_page()

            elif user_prompt_command == 'r':
                break

            # Wait for the rest of the sequence
            elif user_prompt_command.isdigit():
                keyseqs.append(user_prompt_command)
                print(keyseqs)

                # End of the sequence...
                # Two digit sequence -- view artist given number
                if seq_num == 1 and keyseqs[0].isdigit() and keyseqs[1].isdigit():

                    first_num = keyseqs[0]
                    second_num = keyseqs[1]
                    selected_user_num = int(f'{first_num}{second_num}')
                    break  # leave cbreak(), go to gallery

                # Not the end of the sequence yet, continue while block
                else:
                    seq_num += 1

            elif user_prompt_command == 'q':
                print('Are you sure you want to exit?')
                ask_quit()

            elif user_prompt_command == 'm':
                print(main.Users.__doc__)

            elif user_prompt_command == 'h':
                print(''.join([
                    "view ", colors.blue_n, "th artist's illusts ",
                    colors.n, 'ext page; ',
                    colors.p, 'revious page; ',
                    colors.r, 'eload and re-download all;\n',
                    colors.q, 'uit (with confirmation);',
                    'view ', colors.m, 'anual\n'
                ]))

            elif user_prompt_command == '':
                pass
            elif user_prompt_command:
                print('Invalid command! Press h to show help')
                keyseqs = []
                seq_num = 0
            # End if
        # End while
    # End cbreak()

    if user_prompt_command == 'r':
        user_class.reload()
    else:
        user_class.go_artist_mode(selected_user_num)
