"""Handles user interaction inside all the modes. No knowledge of API needed"""

import os
import sys
import threading
from abc import ABC, abstractmethod
from pathlib import Path

import funcy

from koneko import (KONEKODIR, api, data, pure, lscat, utils, colors, config,
                    prompt, download)


def previous_page(data):
    """Previous page for galleries"""
    if data.current_page_num > 1:
        data.current_page_num -= 1

        lscat.show_instant(lscat.TrackDownloads, data, True)

        pure.print_multiple_imgs(data.current_illusts)
        print(f'Page {data.current_page_num}')
        print('Enter a gallery command:\n')

    else:
        print('This is the first page!')

class AbstractGallery(ABC):
    def __init__(self):
        self.data = data.GalleryJson(1, self._main_path)
        self.prefetch_thread: 'threading.Thread'
        # Defined in child classes
        self._main_path: 'Path'

        self.start()

    def start(self):
        """
        If artist_user_id dir exists, show immediately (without checking
        for contents!)
        Else, fetch current_page json and proceed download -> show -> prefetch
        """
        if utils.dir_not_empty(self.data):
            lscat.show_instant(lscat.TrackDownloads, self.data, True)
        elif self.data.download_path.is_dir():
            self.data.download_path.rmdir()

        api.myapi.await_login()
        current_page = self._pixivrequest()
        self.data.update(current_page)

        tracker = lscat.TrackDownloads(self.data)
        # Tracker will cause gallery to show each img as they finish downloading
        download.init_download(self.data, download.download_page, tracker)

        pure.print_multiple_imgs(self.data.current_illusts)
        print(f'Page {self.data.current_page_num}')

        # Make sure the following work:
        # Gallery -> next page -> image prompt -> back -> prev page
        if len(self.data.all_pages_cache) == 1:
            # Prefetch the next page on first gallery load
            self.prefetch_thread = threading.Thread(target=self._prefetch_next_page)
            self.prefetch_thread.start()

    def view_image(self, selected_image_num):
        post_json = self.data.post_json(selected_image_num)
        image_id = post_json.id
        idata = data.ImageJson(post_json, image_id)

        display_image(
            post_json,
            idata.artist_user_id,
            selected_image_num,
            self.data
        )

        # blocking: no way to unblock prompt
        image = Image(image_id, idata, False)
        prompt.image_prompt(image)

        # Image prompt ends, user presses back
        self._back()

    def _back(self):
        """After user 'back's from image prompt or artist gallery, start mode again"""
        lscat.show_instant(lscat.TrackDownloads, self.data, True)
        pure.print_multiple_imgs(self.data.current_illusts)
        print(f'Page {self.data.current_page_num}')
        prompt.gallery_like_prompt(self)

    def next_page(self):
        self.prefetch_thread.join()
        self.data.current_page_num += 1
        if utils.dir_not_empty(self.data):
            lscat.show_instant(lscat.TrackDownloads, self.data, True)
        else:
            print('This is the last page!')
            self.data.current_page_num -= 1
            return False

        pure.print_multiple_imgs(self.data.current_illusts)
        print(f'Page {self.data.current_page_num}')
        print('Enter a gallery command:\n')

        # Skip prefetching again for cases like next -> prev -> next
        if str(self.data.current_page_num + 1) not in self.data.cached_pages:
            self._prefetch_next_page()

    @abstractmethod
    def _pixivrequest(self, **kwargs):
        raise NotImplementedError

    def _prefetch_next_page(self):
        next_url = self.data.next_url
        if not next_url:  # this is the last page
            return

        parse_page = api.myapi.parse_next(next_url)
        next_page = self._pixivrequest(**parse_page)
        self.data.all_pages_cache[str(self.data.current_page_num + 1)] = next_page

        download_path = self._main_path / str(self.data.current_page_num + 1)

        if not download_path.is_dir():
            oldnum = self.data.current_page_num
            self.data.current_page_num += 1
            download.download_page(self.data)
            self.data.current_page_num = oldnum

    def reload(self):
        print('This will delete cached images and redownload them. Proceed?')
        ans = input(f'Directory to be deleted: {self._main_path}\n')
        if ans == 'y' or not ans:
            os.system(f'rm -r {self._main_path}') # shutil.rmtree is better
            self.data.all_pages_cache = {} # Ensures prefetch after reloading
            self.start()
        prompt.gallery_like_prompt(self)
        # Regardless of confirmation, need to catch itself with a prompt

    @abstractmethod
    def handle_prompt(self, keyseqs, gallery_command, selected_image_num,
                      first_num, second_num):
        raise NotImplementedError

    @staticmethod
    @abstractmethod
    def help():
        raise NotImplementedError


class ArtistGallery(AbstractGallery):
    """
    Artist Gallery commands: (No need to press enter)
    Using coordinates, where {x} is the row and {y} is the column
        {x}{y}             -- display the image on row {x} and column {y}
        o{x}{y}            -- open pixiv image/post in browser
        d{x}{y}            -- download image in large resolution

    Using image number, where {number} is the nth image in order (see examples)
        i{number}          -- display the image
        O{number}          -- open pixiv image/post in browser.
        D{number}          -- download image in large resolution.

        n                  -- view the next page
        p                  -- view the previous page
        r                  -- delete all cached images, re-download and reload view
        b                  -- go back to previous mode (either 3, 4, 5, or main screen)
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
    def __init__(self, artist_user_id, **kwargs):
        self._main_path = KONEKODIR / str(artist_user_id)
        self._artist_user_id = artist_user_id
        self._kwargs = kwargs
        super().__init__()

    def _pixivrequest(self, **kwargs):
        if kwargs:
            return api.myapi.artist_gallery_parse_next(**kwargs) # Parse next
        else:
            return api.myapi.artist_gallery_request(self._artist_user_id)

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


class IllustFollowGallery(AbstractGallery):
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
        self._main_path = KONEKODIR / 'illustfollow'
        super().__init__()

    def _pixivrequest(self, **kwargs):
        if kwargs:
            return api.myapi.illust_follow_request(**kwargs) # Parse next
        else:
            return api.myapi.illust_follow_request(restrict='private') # Publicity

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

def display_image(post_json, artist_user_id, number_prefix, data):
    """Image mode, from an artist mode (mode 1/5 -> mode 2)
    Opens image given by the number (medium-res), downloads large-res and
    then display that.
    Alternative to main.view_post_mode(). It does its own stuff before calling
    the Image class for the prompt.

    Unlike the other modes, ui.Image does not handle the initial displaying of images
    This is because coming from a gallery mode, the selected image already has a
    square-medium preview downloaded, which can be displayed before the download
    of the large-res completes. Thus, the initial displaying subroutine will be
    different for a standalone mode or coming from a gallery mode.
    """
    search_string = f"{str(number_prefix).rjust(3, '0')}_*"

    os.system('clear')
    arg = KONEKODIR / str(artist_user_id) / str(data.current_page_num) / search_string
    lscat.icat(arg)

    url = pure.url_given_size(post_json, 'large')
    filename = pure.split_backslash_last(url)
    download_path = (KONEKODIR / str(artist_user_id) / "individual" /
                     str(data.image_id(number_prefix)))
    download.download_core(download_path, url, filename)

    # BLOCKING: imput is blocking, will not display large image until input
    # received

    os.system('clear')
    arg = download_path / filename
    lscat.icat(arg)

def view_post_mode(image_id):
    """Image mode, from main (start -> mode 2)
    Fetch all the illust info, download it in the correct directory, then display it.
    If it is a multi-image post, download the next image
    Else or otherwise, open image prompt
    Unlike the other modes, ui.Image does not handle the initial displaying of images
    This is because coming from a gallery mode, the selected image already has a
    square-medium preview downloaded, which can be displayed before the download
    of the large-res completes. Thus, the initial displaying subroutine will be
    different for a standalone mode or coming from a gallery mode.
    """
    print('Fetching illust details...')
    api.myapi.await_login()
    try:
        post_json = api.myapi.protected_illust_detail(image_id)['illust']
    except KeyError:
        print('Work has been deleted or the ID does not exist!')
        sys.exit(1)

    idata = data.ImageJson(post_json, image_id)

    download.download_core(idata.download_path, idata.current_url, idata.image_filename)
    lscat.icat(idata.download_path / idata.image_filename)
    print(f'Page 1/{idata.number_of_pages}')

    image = Image(image_id, idata, True)

    experimental = config.get_settings('experimental', 'image_mode_previews')
    if experimental == 'on':
        event = threading.Event()
        thread = threading.Thread(target=image.preview)
        image.set_thread_event(thread, event)
        thread.start()

    prompt.image_prompt(image)

class Image:
    """
    Image view commands (No need to press enter):
        b -- go back to the gallery
        n -- view next image in post (only for posts with multiple pages)
        p -- view previous image in post (only for posts with multiple pages)
        d -- download this image in full resolution
        o -- open this post in browser
        f -- show this image in full resolution

        h -- show keybindings
        m -- show this manual
        q -- quit (with confirmation)
    """
    def __init__(self, image_id, idata, firstmode=False):
        self.data = idata
        self._firstmode = firstmode

    def open_image(self):
        utils.open_in_browser(self.data.image_id)

    def download_image(self):
        download.download_url_verified(self.data.current_url)

    def show_full_res(self):
        show_full_res(self.data)

    def next_image(self):
        next_image(self.data)

    def previous_image(self):
        previous_image(self.data)

    def jump_to_image(self, selected_image_num: int):
        jump_to_image(self.data, selected_image_num)

    def _jump(self):
        _jump(self.data)
        self.prefetch_thread = threading.Thread(target=self._prefetch_next_image)
        self.prefetch_thread.start()

    def _prefetch_next_image(self):
        _prefetch_next_image(self.data)

    def leave(self, force=False):
        if self._firstmode or force:
            # Came from view post mode, don't know current page num
            # Defaults to page 1
            mode = ArtistGallery(self.data.artist_user_id)
            prompt.gallery_like_prompt(mode)
        # Else: image prompt and class ends, goes back to previous mode

    def preview(self):
        """Experimental"""
        if self.data.number_of_pages == 1:
            return True

        tracker = lscat.TrackDownloadsImage(self.data)
        slicestart = self.data.page_num
        while not self.event.is_set() and slicestart <= 4:
            img = self.data.page_urls[slicestart:slicestart + 1]

            if os.path.isfile(self.data.download_path / pure.split_backslash_last(img[0])):
                tracker.update(pure.split_backslash_last(img[0]))
            else:
                download.async_download_core(self.data.download_path, img, tracker=tracker)

            slicestart += 1

    def set_thread_event(self, thread, event):
        """Experimental"""
        self.thread = thread
        self.event = event



def show_full_res(data):
    # FIXME: some images that need to be downloaded in png won't work
    # Can use verified function above
    large_url = pure.change_url_to_full(url=data.current_url)
    filename = pure.split_backslash_last(large_url)
    download.download_core(data.download_path, large_url, filename)
    lscat.icat(data.download_path / filename)

def next_image(data):
    if not data.page_urls:
        print('This is the only page in the post!')
        return False
    elif data.page_num + 1 == data.number_of_pages:
        print('This is the last image in the post!')
        return False

    # jump_to_image corrects for 1-based
    data.page_num += 1
    jump_to_image(data, data.page_num + 1)

def previous_image(data):
    if not data.page_urls:
        print('This is the only page in the post!')
        return False
    elif data.page_num == 0:
        print('This is the first image in the post!')
        return False

    data.page_num -= 1
    jump_to_image(data, data.page_num + 1)

def jump_to_image(data, selected_image_num: int):
    if selected_image_num <= 0 or selected_image_num > len(data.page_urls):
        print("Invalid number!")
        return False

    # Internally 0-based, but externally 1-based
    data.page_num = selected_image_num - 1
    _jump(data)

def _jump(data):
    """Downloads next image if not downloaded, display it, prefetch next"""
    if not (data.download_path / data.image_filename).is_dir():
        download.async_download_spinner(data.download_path,
                                        [data.current_url])

    lscat.icat(data.filepath)

    print(f'Page {data.page_num+1}/{data.number_of_pages}')

def _prefetch_next_image(data):
    with funcy.suppress(IndexError):
        next_img_url = data.next_img_url
    if next_img_url:
        download.async_download_spinner(data.download_path, [next_img_url])



def previous_page_users(data):
    """Previous page for users"""
    if data.page_num > 1:
        data.page_num -= 1
        data.offset = int(data.offset) - 30
        _show_page(data)
    else:
        print('This is the first page!')

def _show_page(data):
    if not utils.dir_not_empty(data):
        print('This is the last page!')
        data.page_num -= 1
        return False

    lscat.show_instant(lscat.TrackDownloadsUsers, data)

class AbstractUsers(ABC):
    """
    User view commands (No need to press enter):
        {x}{y}             -- display artist illusts on column {x} and row {y}
        n                  -- view next page
        p                  -- view previous page
        r                  -- delete all cached images, re-download and reload view
        h                  -- show keybindings
        m                  -- show this manual
        q                  -- quit (with confirmation)

    """

    @abstractmethod
    def __init__(self, user_or_id):
        self.data: 'data.UserJson'
        self._input = user_or_id
        self.prefetch_thread: 'threading.Thread'
        # Defined in child classes
        self._main_path: 'Path'

    def start(self):
        self.data = data.UserJson(1, self._main_path, self._input)
        self._parse_and_download()
        self.prefetch_thread = threading.Thread(target=self._prefetch_next_page)
        self.prefetch_thread.start()

    def _parse_and_download(self):
        """If download path not empty, immediately show. Else parse & download"""
        if utils.dir_not_empty(self.data):
            lscat.show_instant(lscat.TrackDownloadsUsers, self.data)
            api.myapi.await_login()
            self.parse_thread = threading.Thread(target=self._parse_user_infos)
            self.parse_thread.start()
            return True

        # No valid cached images, download all from scratch
        if self.data.download_path.is_dir():
            self.data.download_path.rmdir()

        api.myapi.await_login()
        self._parse_user_infos()
        tracker = lscat.TrackDownloadsUsers(self.data)
        download.init_download(self.data, download.user_download, tracker)

    @abstractmethod
    def _pixivrequest(self):
        """Blank method, classes that inherit this ABC must override this"""
        raise NotImplementedError

    @utils.spinner('Parsing info...')
    def _parse_user_infos(self):
        """Parse json and get list of artist names, profile pic urls, and id"""
        result = self._pixivrequest()
        if not hasattr(self, 'data'):
            self.data = data.UserJson(1, self._main_path, self._input)
        else:
            self.data.update(result)

    def _show_page(self):
        _show_page(self.data)

    def _prefetch_next_page(self):
        # TODO: split into download and data parts
        # Wait for initial request to finish, so the data object is instantiated
        if hasattr(self, 'parse_thread'):
            self.parse_thread.join()
        oldnum = self.data.page_num

        if self.data.next_url:
            next_offset = api.myapi.parse_next(self.data.next_url)['offset']
            # Won't download if not immediately next page, eg
            # p1 (p2 downloaded) -> p2 (p3) -> p1 -> p2 (p4 won't download)
            if int(next_offset) - int(self.data.offset) <= 30:
                self.data.offset = next_offset
                self.data.page_num = int(self.data.offset) // 30 + 1

                self._parse_user_infos()
                download.init_download(self.data, download.user_download, None)

        self.data.page_num = oldnum

    def next_page(self):
        self.prefetch_thread.join()
        self.data.page_num += 1
        self._show_page()

        self._prefetch_next_page()

    def previous_page(self):
        previous_page_users(self.data)

    def go_artist_mode(self, selected_user_num):
        try:
            artist_user_id = self.data.artist_user_id(selected_user_num)
        except IndexError:
            print('Invalid number!')
        else:
            mode = ArtistGallery(artist_user_id)
            prompt.gallery_like_prompt(mode)
            # After backing from gallery
            self._show_page()
            prompt.user_prompt(self)

    def reload(self):
        print('This will delete cached images and redownload them. Proceed?')
        ans = input(f'Directory to be deleted: {self._main_path}\n')
        if ans == 'y' or not ans:
            os.system(f'rm -r {self.data.main_path}') # shutil.rmtree is better
            self.__init__(self.data._input)
            self.start()
        prompt.user_prompt(self)


class SearchUsers(AbstractUsers):
    """
    Inherits from AbstractUsers class, define self._input as the search string (user)
    Parent directory for downloads should go to search/
    """
    def __init__(self, user):
        self._main_path = KONEKODIR / 'search'
        super().__init__(user)

    def _pixivrequest(self):
        return api.myapi.search_user_request(self._input, self.data.offset)

class FollowingUsers(AbstractUsers):
    """
    Inherits from AbstractUsers class, define self._input as the user's pixiv ID
    (Or any other pixiv ID that the user wants to look at their following users)
    Parent directory for downloads should go to following/
    """
    def __init__(self, your_id, publicity='private'):
        self._publicity = publicity
        self._main_path = KONEKODIR / 'following'
        super().__init__(your_id)

    def _pixivrequest(self):
        return api.myapi.following_user_request(self._input, self._publicity,
                                                self.data.offset)
