import os
import sys
from collections import namedtuple

from koneko import lscat, TERM


# Duplicated due to circular import
FakeData = namedtuple('data', ('download_path',))


def gallery_user_loop(data, cls):
    show_images = True
    max_pages = len([x for x in os.listdir(data.download_path.parent)
                     if x.isdigit()])

    with TERM.cbreak():
        while True:
            current_page = int(data.download_path.name)

            if show_images:
                lscat.show_instant(cls, data)
                print(f'Page {current_page} / {max_pages}')
                print("Press 'n' to go to next page, "
                        "'p' to go to the previous page, "
                        "and 'q' to quit")

            ans = TERM.inkey()
            print(ans)

            if ans == 'n' and current_page == max_pages:
                print("This is the last cached page!")
                show_images = False

            elif ans == 'p' and current_page == 1:
                print("This is the last page!")
                show_images = False

            elif ans == 'n':
                current_page += 1
                show_images = True

            elif ans == 'p':
                current_page -= 1
                show_images = True

            elif ans == 'q':
                sys.exit(0)

            else:
                print("Inshow_images input!")
                show_images = False

            if show_images:
                data = FakeData(data.download_path.parent / str(current_page))
