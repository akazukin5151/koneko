import os
import sys
import threading
from abc import ABC, abstractmethod
from pathlib import Path

import funcy

from koneko import (KONEKODIR, api, data, pure, lscat, utils, colors, config,
                    prompt, download, ui)


def previous_page_gallery(data):
    """Previous page for users"""
    if data.page_num > 1:
        data.page_num -= 1
        data.offset = int(data.offset) - 30
        _show_page_gallery(data)
    else:
        print('This is the first page!')

def _show_page_gallery(data):
    if not utils.dir_not_empty(data):
        print('This is the last page!')
        data.page_num -= 1
        return False

    lscat.show_instant(lscat.TrackDownloads, data)
    # New
    pure.print_multiple_imgs(self.data.current_illusts)
    print(f'Page {self.data.current_page_num}')

class AbstractGalleryNew(ABC):
    @abstractmethod
    def __init__(self, main_path):
        self.data: 'data.GalleryJson'
        self.start(main_path)

    # New; should be implemented in Users
    def _prefetch_thread(self):
        """Reassign the thread again and start; as threads can only be started once"""
        self.prefetch_thread = threading.Thread(target=self._prefetch_next_page)
        self.prefetch_thread.start()

    def start(self, main_path):
        self.data = data.GalleryJson(1, main_path)
        self._parse_and_download()
        self._prefetch_thread()

    def _parse_and_download(self):
        """If download path not empty, immediately show. Else parse & download"""
        if utils.dir_not_empty(self.data):
            lscat.show_instant(lscat.TrackDownloads, self.data, True)
            api.myapi.await_login()
            # Parse in the background; the other branch doesn't need this
            self._parse_user_infos()
            # Might as well put this in a function
            pure.print_multiple_imgs(self.data.current_illusts)
            print(f'Page {self.data.current_page_num}')
            return True

        # No valid cached images, download all from scratch
        if self.data.download_path.is_dir():
            self.data.download_path.rmdir()

        api.myapi.await_login()
        self._parse_user_infos()
        tracker = lscat.TrackDownloads(self.data)
        download.init_download(self.data, download.download_page, tracker)
        # New
        pure.print_multiple_imgs(self.data.current_illusts)
        print(f'Page {self.data.current_page_num}')

    @abstractmethod
    def _pixivrequest(self):
        """Blank method, classes that inherit this ABC must override this"""
        raise NotImplementedError

    def _parse_user_infos(self):
        """Parse json and get list of artist names, profile pic urls, and id"""
        result = self._pixivrequest()
        self.data.update(result)

    def _show_page(self):
        _show_page_gallery(self.data)

    def _prefetch_next_page(self):
        # Wait for initial request to finish, so the data object is instantiated
        # Else next_url won't be set yet
        if not self.data.next_url:  # Last page
            return True

        next_offset = self.data.next_url.split('&')[-1].split('=')[-1]
        # Won't download if not immediately next page, eg
        # p1 (p2 prefetched) -> p2 (p3) -> p1 -> p2 (p4 won't prefetch)
        offset_diffs = int(next_offset) - int(self.data.offset)
        immediate_next: bool = offset_diffs <= 30
        if not immediate_next:
            return

        oldnum = self.data.page_num
        self.data.offset = next_offset
        self.data.page_num = int(self.data.offset) // 30 + 1

        self._parse_user_infos()
        download.init_download(self.data, download.download_page, None)

        self.data.page_num = oldnum

    def next_page(self):
        self.prefetch_thread.join()
        self.data.page_num += 1
        self._show_page()
        self._prefetch_next_page()

    def previous_page(self):
        previous_page_gallery(self.data)

    # Entire block removed
    #def go_artist_mode(self, selected_user_num):

    def reload(self):
        print('This will delete cached images and redownload them. Proceed?')
        ans = input(f'Directory to be deleted: {self.data.main_path}\n')
        if ans == 'y' or not ans:
            os.system(f'rm -r {self.data.main_path}') # shutil.rmtree is better
            # Will remove all data, but keep main path and user input
            self.start(self.data.main_path)
        prompt.user_prompt(self)

    # New; should be in abstract gallery
    @abstractmethod
    def handle_prompt(self, keyseqs):
        raise NotImplementedError

    @staticmethod
    @abstractmethod
    def help():
        raise NotImplementedError

    # New; same
    def view_image(self, selected_image_num):
        post_json = self.data.post_json(selected_image_num)
        image_id = post_json.id
        idata = data.ImageJson(post_json, image_id)

        ui.display_image(
            post_json,
            idata.artist_user_id,
            selected_image_num,
            self.data
        )

        # blocking: no way to unblock prompt
        image = ui.Image(image_id, idata, False)
        prompt.image_prompt(image)

        # Image prompt ends, user presses back
        self._back()

    def _back(self):
        """After user 'back's from image prompt or artist gallery, start mode again"""
        lscat.show_instant(lscat.TrackDownloads, self.data, True)
        pure.print_multiple_imgs(self.data.current_illusts)
        print(f'Page {self.data.current_page_num}')
        prompt.gallery_like_prompt(self)


class ArtistGallery(AbstractGalleryNew):
    def __init__(self, artist_user_id, **kwargs):
        self._artist_user_id = artist_user_id
        self._kwargs = kwargs
        super().__init__(KONEKODIR / str(artist_user_id))

    def _pixivrequest(self, **kwargs):
        return api.myapi.artist_gallery_general(self._artist_user_id, self.data.offset)

    def handle_prompt(self, keyseqs):
        # Display image (using either coords or image number), the show this prompt
        if keyseqs[0] == 'b':
            pass # Stop gallery instance, return to previous state
        elif keyseqs[0] == 'r':
            self.reload()
        elif keyseqs[0].lower() == 'a':
            print('Invalid command! Press h to show help')
            prompt.gallery_like_prompt(self) # Go back to while loop

    @staticmethod
    def help():
        print(''.join(
            colors.base1 + ['view '] + colors.base2 +
            ['view ', colors.m, 'anual; ',
              colors.b, 'ack\n']))

class IllustFollowGallery(AbstractGalleryNew):
    """
    Illust Follow Gallery commands: (No need to press enter)
    Using coordinates, where {x} is the row and {y} is the column
        {x}{y}             -- display the image on row {x} and column {y}
        o{x}{y}            -- open pixiv image/post in browser
        d{x}{y}            -- download image in large resolution
        a{x}{y}            -- view illusts by the artist of the selected image

    Using image number, where {number} is the nth image in order (see examples)
        i{number}          -- display the image
        O{number}          -- open pixiv image/post in browser.
        D{number}          -- download image in large resolution.
        A{number}          -- view illusts by the artist of the selected image

        n                  -- view the next page
        p                  -- view the previous page
        r                  -- delete all cached images, re-download and reload view
        b                  -- go back to main screen
        h                  -- show keybindings
        m                  -- show this manual
        q                  -- quit (with confirmation)

    Examples:
        i09   --->  Display the ninth image in image view (must have leading 0)
        i10   --->  Display the tenth image in image view
        O29   --->  Open the last image's post in browser
        D00   --->  Download the first image, in large resolution

        25    --->  Display the image on column 2, row 5 (index starts at 1)
        d25   --->  Open the image on column 2, row 5 (index starts at 1) in browser
        o25   --->  Download the image on column 2, row 5 (index starts at 1)
    """
    def __init__(self):
        super().__init__(KONEKODIR / 'illustfollow')

    def _pixivrequest(self):
        return api.myapi.illust_follow_request(restrict='private',
                                               offset=self.data.offset)

    def go_artist_gallery_coords(self, first_num, second_num):
        selected_image_num = utils.find_number_map(int(first_num), int(second_num))
        if selected_image_num is False: # 0 is valid!
            print('Invalid number!')
        else:
            self.go_artist_gallery_num(selected_image_num)

    def go_artist_gallery_num(self, selected_image_num):
        """Like self.view_image(), but goes to artist mode instead of image"""
        artist_user_id = self.data.artist_user_id(selected_image_num)
        mode = ArtistGallery(artist_user_id)
        prompt.gallery_like_prompt(mode)
        # Gallery prompt ends, user presses back
        self._back()

    def handle_prompt(self, keyseqs):
        # "b" must be handled first, because keyseqs might be empty
        if keyseqs[0] == 'b':
            print('Invalid command! Press h to show help')
            prompt.gallery_like_prompt(self) # Go back to while loop
        elif keyseqs[0] == 'r':
            self.reload()
        elif keyseqs[0] == 'a':
            self.go_artist_gallery_coords(*keyseqs[-2:])
        elif keyseqs[0] == 'A':
            self.go_artist_gallery_num(utils.process_digits(keyseqs))

    @staticmethod
    def help():
        print(''.join(colors.base1 + [
            colors.a, "view artist's illusts; ",
            colors.n, 'ext page;\n',
            colors.p, 'revious page; ',
            colors.r, 'eload and re-download all; ',
            colors.q, 'uit (with confirmation); ',
            'view ', colors.m, 'anual\n']))
