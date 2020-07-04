"""Handles user interaction inside all the modes. No knowledge of API needed"""

import os
import sys
import threading
from shutil import rmtree
from abc import ABC, abstractmethod

import funcy

from koneko import (
    api,
    data,
    pure,
    lscat,
    utils,
    files,
    colors,
    config,
    prompt,
    printer,
    download,
    KONEKODIR
)


class AbstractUI(ABC):
    @abstractmethod
    def __init__(self, main_path) -> 'IO':
        """Child classes must pass in main_path, and
        declare the data attribute as appropriate.
        Main path includes any user input (eg, artist user id or search string)
        """
        self.data: 'data.<class>'  # Define in self.data_class(), not __init__
        self.start(main_path)

    @abstractmethod
    def data_class(self, main_path) -> None:
        """Instantiate the appropriate data object here (with args and bracket)"""
        raise NotImplementedError

    @abstractmethod
    def tracker(self) -> None:
        """Instantiate the appropriate tracker object here (with args and bracket)"""
        raise NotImplementedError

    @abstractmethod
    def show_instant(self) -> 'IO':
        """Run the appropriate lscat.show_instant function here
        (will actually display)
        """
        raise NotImplementedError

    @abstractmethod
    def _pixivrequest(self) -> 'Json':
        """Run the appropriate api request function and return the result
        Must pass in `offset=self.data.offset` into API for pages to work
        """
        raise NotImplementedError

    @abstractmethod
    def maybe_join_thread(self) -> None:
        """Run any procedure before prefetching (either in background or not)"""
        raise NotImplementedError

    @abstractmethod
    def print_page_info(self) -> None:
        """Run any procedure to print the page info"""
        raise NotImplementedError

    def start(self, main_path: 'Path') -> 'IO':
        # self.data defined here not in __init__, so that reload() will wipe cache
        self.data = self.data_class(main_path)
        self._parse_and_download()
        self._prefetch_thread()

    def verify_up_to_date(self):
        if files.dir_not_empty(self.data):
            return True
        files.remove_dir_if_exist(self.data)
        download.init_download(self.data, self.tracker())

    def _parse_and_download(self) -> 'IO':
        """If download path not empty, immediately show.
        Regardless, proceed to parse & download
        Before fetching, show the dir first. Can only check for 'is dir' and 'not empty'
        After fetching, double check all files in dir match the cache
        """
        if files.dir_not_empty(self.data):
            self.show_instant()
            self._parse_user_infos()
            self.verify_up_to_date()
            self.print_page_info()
            return True

        # No valid cached images, download all from scratch
        files.remove_dir_if_exist(self.data)

        self._parse_user_infos()
        download.init_download(self.data, self.tracker())
        self.print_page_info()

    def _prefetch_thread(self) -> 'IO':
        """Reassign the thread again and start; as threads can only be started once"""
        self.prefetch_thread = threading.Thread(target=self._prefetch_next_page)
        self.prefetch_thread.start()

    def _parse_user_infos(self) -> 'IO':
        """Parse json and get list of artist names, profile pic urls, and id"""
        result = self._pixivrequest()
        self.data.update(result)

    def _prefetch_next_page(self) -> 'IO':
        # Wait for initial request to finish, so the data object is instantiated
        # Else next_url won't be set yet
        self.maybe_join_thread()
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
        download.init_download(self.data, None)

        self.data.page_num = oldnum

    def next_page(self) -> 'IO':
        self.prefetch_thread.join()
        self.data.page_num += 1
        self._show_page()
        self._prefetch_next_page()

    def previous_page(self) -> 'IO':
        if self.data.page_num > 1:
            self.data.page_num -= 1
            self.data.offset = int(self.data.offset) - 30
            self._show_page()
            return True
        print('This is the first page!')

    def _show_page(self) -> 'IO':
        if not files.dir_not_empty(self.data):
            print('This is the last page!')
            self.data.page_num -= 1
            return False
        self.show_instant()
        self.print_page_info()

    def reload(self) -> 'IO':
        print('This will delete cached images and redownload them. Proceed?')
        ans = input(f'Directory to be deleted: {self.data.main_path}\n')
        if ans == 'y' or not ans:
            rmtree(self.data.main_path)
            # Will remove all data, but keep info on the main path
            self.start(self.data.main_path)
        prompt.user_prompt(self)


class AbstractGallery(AbstractUI, ABC):
    @abstractmethod
    def __init__(self, main_path):
        """Complements abstractmethod: Define download function for galleries"""
        super().__init__(main_path)

    def data_class(self, main_path):
        """Implements abstractmethod: Instantiate the dataclass for galleries"""
        return data.GalleryData(1, main_path)

    def tracker(self):
        """Implements abstractmethod: Instantiate tracker for galleries"""
        return lscat.TrackDownloads(self.data)

    def show_instant(self):
        """Implements abstractmethod: Runs show_instant for galleries"""
        return lscat.show_instant(lscat.TrackDownloads, self.data, True)

    def maybe_join_thread(self):
        """Implements abstractmethod: No action needed"""
        return True

    def print_page_info(self):
        """Implements abstractmethod: Indicate which posts are multi-image and
        current page number
        """
        printer.print_multiple_imgs(self.data.current_illusts)
        print(f'Page {self.data.page_num}')

    # Unique for Galleries
    @abstractmethod
    def handle_prompt(self, keyseqs: 'list[str]') -> 'IO':
        """Abstractmethod for gallery classes: Gallery prompt accepts more
        keys(eqs) than Users, handle them here
        """
        raise NotImplementedError

    @staticmethod
    @abstractmethod
    def help() -> 'IO':
        """Abstractmethod for gallery classes: each gallery mode has different
        keyseqs and thus help
        """
        raise NotImplementedError

    def view_image(self, selected_image_num: int) -> 'IO':
        """Image mode, from an artist mode (mode 1/5 -> mode 2)
        Display already downloaded preview (medium-res), downloads large-res and
        then display that, finally launch the image prompt.
        Alternative to main.view_post_mode(). It does its own stuff before calling
        the Image class for the prompt.

        Unlike the other modes, Image does not handle the initial displaying of images
        This is because coming from a gallery mode, the selected image already has a
        square-medium preview downloaded, which can be displayed before the download
        of the large-res completes. Thus, the initial displaying subroutine will be
        different for a standalone mode or coming from a gallery mode.
        """
        post_json = self.data.post_json(selected_image_num)
        image_id = post_json.id
        idata = data.ImageData(post_json, image_id)

        _display_medium_preview(self.data, idata, selected_image_num)

        download.download_url(idata.download_path, idata.page_urls[0],
                              idata.large_filename)

        lscat.icat(idata.download_path / idata.large_filename)

        image = Image(image_id, idata, False)
        prompt.image_prompt(image)

        # Image prompt ends, user presses back
        self._back()

    def _back(self) -> 'IO':
        """After user 'back's from image prompt or artist gallery, start mode again"""
        self.show_instant()
        self.print_page_info()
        prompt.gallery_like_prompt(self)


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
    def __init__(self, artist_user_id):
        """Implements abstractmethod: self._artist_user_id only used for
        _pixivrequest() specific to mode 1
        """
        self._artist_user_id = artist_user_id
        super().__init__(KONEKODIR / str(artist_user_id))

    def _pixivrequest(self):
        """Implements abstractmethod: use the user-given id for request"""
        return api.myapi.artist_gallery(self._artist_user_id, self.data.offset)

    def handle_prompt(self, keyseqs):
        """Implements abstractmethod"""
        # Display image (using either coords or image number), the show this prompt
        if keyseqs[0] == 'b':
            pass  # Stop gallery instance, return to previous state
        elif keyseqs[0] == 'r':
            self.reload()
        elif keyseqs[0].lower() == 'a':
            print('Invalid command! Press h to show help')
            prompt.gallery_like_prompt(self)  # Go back to while loop

    @staticmethod
    def help():
        """Implements abstractmethod"""
        print('')
        print(''.join(
            colors.base1 + ['view '] + colors.base2
            + ['view ', colors.m, 'anual; ',
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
        """Implements abstractmethod"""
        super().__init__(KONEKODIR / 'illustfollow')

    def _pixivrequest(self):
        """Implements abstractmethod, publicity is private for now
        (might be configurable in the future)
        """
        return api.myapi.illust_follow_request(restrict='private',
                                               offset=self.data.offset)

    def go_artist_gallery_coords(self, first_num, second_num: str) -> 'IO':
        """New method for mode 5 only"""
        selected_image_num = utils.find_number_map(int(first_num), int(second_num))
        if selected_image_num is False:  # 0 is valid!
            print('Invalid number!')
        else:
            self.go_artist_gallery_num(selected_image_num)

    def go_artist_gallery_num(self, selected_image_num: int) -> 'IO':
        """Like self.view_image(), but goes to artist mode instead of image"""
        artist_user_id = self.data.artist_user_id(selected_image_num)
        mode = ArtistGallery(artist_user_id)
        prompt.gallery_like_prompt(mode)
        # Gallery prompt ends, user presses back
        self._back()

    def handle_prompt(self, keyseqs):
        """Implements abstractmethod"""
        # "b" must be handled first, because keyseqs might be empty
        if keyseqs[0] == 'b':
            print('Invalid command! Press h to show help')
            prompt.gallery_like_prompt(self)  # Go back to while loop
        elif keyseqs[0] == 'r':
            self.reload()
        elif keyseqs[0] == 'a':
            self.go_artist_gallery_coords(*keyseqs[-2:])
        elif keyseqs[0] == 'A':
            self.go_artist_gallery_num(pure.concat_seqs_to_int(keyseqs, 1))

    @staticmethod
    def help():
        """Implements abstractmethod"""
        print('')
        print(''.join(colors.base1 + [
            colors.a, "view artist's illusts; ",
            colors.n, 'ext page;\n',
            colors.p, 'revious page; ',
            colors.r, 'eload and re-download all; ',
            colors.q, 'uit (with confirmation); ',
            'view ', colors.m, 'anual\n']))


class AbstractUsers(AbstractUI, ABC):
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
    def __init__(self, main_path):
        """Complements abstractmethod: Define download function for user modes"""
        super().__init__(main_path)

    def data_class(self, main_path):
        """Implements abstractmethod: Instantiate the dataclass for user modes"""
        return data.UserData(1, main_path)

    def tracker(self):
        """Implements abstractmethod: Instantiate tracker for user modes"""
        return lscat.TrackDownloadsUsers(self.data)

    def show_instant(self):
        """Implements abstractmethod: Runs show_instant for user modes"""
        return lscat.show_instant(lscat.TrackDownloadsUsers, self.data)

    def maybe_join_thread(self):
        """Implements abstractmethod: Wait for parse_thread to join (if any)"""
        with funcy.suppress(AttributeError):
            self.parse_thread.join()

    def print_page_info(self):
        """Implements abstractmethod: Indicate current page number"""
        print(f'Page {self.data.page_num}')

    # Unique to Users
    def go_artist_mode(self, selected_user_num: int) -> 'IO':
        """Concrete method unique for both user modes"""
        try:
            artist_user_id = self.data.artist_user_id(selected_user_num)
        except IndexError:
            print('Invalid number!')
            return False

        mode = ArtistGallery(artist_user_id)
        prompt.gallery_like_prompt(mode)
        # After backing from gallery
        self._show_page()
        prompt.user_prompt(self)


class SearchUsers(AbstractUsers):
    """
    Inherits from AbstractUsers class, define self._input as the search string (user)
    Parent directory for downloads should go to search/
    """
    def __init__(self, user):
        self.user = user  # This is only used for pixivrequest
        super().__init__(KONEKODIR / 'search' / user)

    def _pixivrequest(self):
        return api.myapi.search_user_request(self.user, self.data.offset)


class FollowingUsers(AbstractUsers):
    """
    Inherits from AbstractUsers class, define self._input as the user's pixiv ID
    (Or any other pixiv ID that the user wants to look at their following users)
    Parent directory for downloads should go to following/
    """
    def __init__(self, your_id, publicity='private'):
        """Implements abstractmethod, publicity is private for now
        (might be configurable in the future)
        """
        self._publicity = publicity
        self.your_id = your_id
        super().__init__(KONEKODIR / 'following' / your_id)

    def _pixivrequest(self):
        return api.myapi.following_user_request(
            self.your_id, self._publicity, self.data.offset
        )


def _display_medium_preview(gdata, idata, num: int) -> 'IO':
    os.system('clear')
    image = sorted(os.listdir(gdata.download_path))[num]
    lscat.icat(gdata.main_path / str(gdata.page_num) / image)


def view_post_mode(image_id) -> 'IO':
    """Image mode, from main (start -> mode 2)
    Fetch all the illust info, download it in the correct directory, then display it.
    If it is a multi-image post, download the next image
    Else or otherwise, open image prompt
    Unlike the other modes, Image does not handle the initial displaying of images
    This is because coming from a gallery mode, the selected image already has a
    square-medium preview downloaded, which can be displayed before the download
    of the large-res completes. Thus, the initial displaying subroutine will be
    different for a standalone mode or coming from a gallery mode.
    """
    print('Fetching illust details...')
    try:
        post_json = api.myapi.protected_illust_detail(image_id)['illust']
    except KeyError:
        print('Work has been deleted or the ID does not exist!')
        sys.exit(1)

    idata = data.ImageData(post_json, image_id)

    download.download_url(idata.download_path, idata.current_url, idata.image_filename)

    lscat.icat(idata.download_path / idata.image_filename)

    print(f'Page 1/{idata.number_of_pages}')

    image = Image(image_id, idata, True)

    image.start_preview()

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
        self.event = threading.Event()
        self.thread: 'threading.Thread'
        self._firstmode = firstmode

    def open_image(self) -> 'IO':
        utils.open_in_browser(self.data.image_id)

    def download_image(self) -> 'IO':
        download.download_url_verified(self.data.current_url)

    def show_full_res(self) -> 'IO':
        show_full_res(self.data)

    def next_image(self) -> 'IO':
        self.event.set()
        next_image(self.data)
        self.start_preview()

    def previous_image(self) -> 'IO':
        self.event.set()
        previous_image(self.data)
        self.start_preview()

    def jump_to_image(self, selected_image_num: int) -> 'IO':
        self.event.set()
        jump_to_image(self.data, selected_image_num)
        self.start_preview()

    def _jump(self) -> 'IO':
        _jump(self.data)
        self.prefetch_thread = threading.Thread(target=self._prefetch_next_image)
        self.prefetch_thread.start()

    def _prefetch_next_image(self) -> 'IO':
        _prefetch_next_image(self.data)

    def leave(self, force=False) -> 'IO':
        self.event.set()
        if self._firstmode or force:
            # Came from view post mode, don't know current page num
            # Defaults to page 1
            mode = ArtistGallery(self.data.artist_user_id)
            prompt.gallery_like_prompt(mode)
        # Else: image prompt and class ends, goes back to previous mode

    def start_preview(self):
        if config.check_image_preview() and self.data.number_of_pages > 1:
            self.event = threading.Event()  # Reset event, in case if it's set
            self.thread = threading.Thread(target=self.preview)
            self.thread.start()

    def preview(self) -> 'IO':
        """Download the next four images in the background and/or display them
        one at a time, so if user interrupts, it won't hang.
        """
        tracker = lscat.TrackDownloadsImage(self.data)
        i = 1
        while not self.event.is_set() and i <= 4:
            url = self.data.page_urls[self.data.page_num + i]
            name = pure.split_backslash_last(url)
            path = self.data.download_path / name

            if path.is_file():
                tracker.update(name)
            else:
                download.async_download_no_rename(
                    self.data.download_path, [url], tracker=tracker
                )

            if i == 4:  # Last pic
                print('\n' * config.image_text_offset())

            i += 1


def show_full_res(data):
    # FIXME: some images that need to be downloaded in png won't work
    # Can use verified function above
    large_url = pure.change_url_to_full(data.current_url)
    filename = pure.split_backslash_last(large_url)
    download.download_url(data.download_path, large_url, filename)
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
        print('Invalid number!')
        return False

    # Internally 0-based, but externally 1-based
    data.page_num = selected_image_num - 1
    _jump(data)


def _jump(data):
    """Downloads next image if not downloaded, display it, prefetch next"""
    if not (data.download_path / data.image_filename).is_dir():
        download.async_download_spinner(
            data.download_path, [data.current_url]
        )

    os.system('clear')

    lscat.icat(data.filepath)

    print(f'Page {data.page_num+1}/{data.number_of_pages}')


def _prefetch_next_image(data):
    with funcy.suppress(IndexError):
        next_img_url = data.next_img_url
    if next_img_url:
        download.async_download_spinner(data.download_path, [next_img_url])

