"""Handles user interaction inside all the modes. No knowledge of API needed"""

import os
import sys
import threading
from shutil import rmtree
from contextlib import suppress
from abc import ABC, abstractmethod

from koneko import (
    api,
    data,
    pure,
    TERM,
    lscat,
    utils,
    files,
    colors,
    config,
    prompt,
    printer,
    download,
    KONEKODIR,
)


class AbstractUI(ABC):
    @abstractmethod
    def __init__(self, main_path) -> 'IO':
        """Child classes must pass in main_path, and
        declare the data attribute as appropriate.
        Main path includes any user input (eg, artist user id or search string)
        """
        self._max_images: int
        self._is_gallery_mode: bool
        # Reference to the appropriate class or function
        self._prompt: 'prompt.<function>'
        self._data_class: 'data.<class>'
        self._tracker_class: 'lscat.<class>'

        # Attribute defined in self.start()
        self._data: 'data.<class>'  # Instantiated data class, not reference
        self.images: 'list[Image]' = []
        # Attribute defined in self.prefetch_thread()
        self._prefetch_thread: threading.Thread

        self.use_ueberzug = config.api.use_ueberzug()
        self.scrollable = self.use_ueberzug or not config.api.scroll_display()
        self.terminal_page = 0  # starts at zero so the first slice has start=0
        self.start(main_path)

    @abstractmethod
    def _pixivrequest(self) -> 'Json':
        """Run the appropriate api request function and return the result
        Must pass in `offset=self._data.offset` into API for pages to work
        """
        raise NotImplementedError

    def _maybe_join_thread(self) -> None:
        """Run any procedure before prefetching (either in background or not)"""
        return True

    @abstractmethod
    def _print_page_info(self) -> None:
        """Run any procedure to print the page info"""
        raise NotImplementedError

    def _report(self):
        with printer.maybe_print_bottom(self.use_ueberzug):
            self._print_page_info()

    def start(self, main_path: 'Path') -> 'IO':
        # self._data defined here not in __init__, so that reload() will wipe cache
        # This has to be taken into account before any attempts to make this a subclass of Data
        self._data = self._data_class(main_path)
        if self._data.download_path.is_dir() and os.listdir(self._data.download_path):
            self._show_then_fetch()
        else:
            self._download_from_scratch()
        self._prefetch()

    def _download_from_scratch(self) -> 'IO':
        files.remove_dir_if_exist(self._data)
        self._request_then_save()
        self._download_save_images()
        self._report()

    def _show_then_fetch(self) -> 'IO':
        self.scroll_or_show()
        self._request_then_save()
        self._verify_up_to_date()
        self._report()

    def _verify_up_to_date(self) -> 'IO':
        if files.dir_not_empty(self._data):
            return True
        files.remove_dir_if_exist(self._data)
        self._download_save_images()

    def _download_save_images(self):
        tracker = self._tracker_class(self._data)
        download.init_download(self._data, tracker)
        self.images = tracker.images

    def _prefetch(self) -> 'IO':
        """Reassign the thread again and start; as threads can only be started once"""
        self._prefetch_thread = threading.Thread(target=self._prefetch_next_page)
        self._prefetch_thread.start()

    def _request_then_save(self, page_num=None) -> 'IO':
        """Do request and save it"""
        result = self._pixivrequest()
        if page_num:
            self._data.update(result, page_num)
        else:
            self._data.update(result)

    def _prefetch_next_page(self) -> 'IO':
        # Wait for initial request to finish, so the data object is instantiated
        # Else next_url won't be set yet
        self._maybe_join_thread()

        if self._data.next_url is None:
            return True
        # For some reason, if next_url is None, this statement doesn't catch it (???)
        if not (
            self._data.next_url
            or self._data.next_offset.isdigit()
            or self._data.is_immediate_next
        ):
            return True

        next_offset = self._data.next_offset
        if self._data.offset == next_offset:
            return
        self._data.offset = next_offset
        self._request_then_save(self._data.page_num + 1)
        new = self._data.clone_with_page(int(self._data.offset) // 30 + 1)
        download.init_download(new, None)

    def next_page(self) -> 'IO':
        print('Downloading images in the next page...')
        self._prefetch_thread.join()
        self._data.page_num += 1
        self.terminal_page = 0
        self._show_page()
        self._prefetch()

    def previous_page(self) -> 'IO':
        if self._data.page_num <= 1:
            printer.print_bottom('This is the first page!')
            return False
        self._data.page_num -= 1
        self.terminal_page = 0
        self._data.offset = int(self._data.offset) - 30
        self._show_page()

    def scroll_up(self):
        if self.terminal_page == 0:
            printer.print_bottom('This is the top of the terminal page!')
            return False

        self.terminal_page -= 1
        self._show_page()

    def scroll_down(self):
        if self.terminal_page + 1 >= utils.max_terminal_scrolls(
            self._data, self._is_gallery_mode
        ):
            printer.print_bottom('This is the bottom of the terminal page!')
            return False

        self.terminal_page += 1
        self._show_page()

    def handle_scroll(self):
        lscat.api.hide_all(self.images)
        myslice = utils.slice_images(self._max_images, self.terminal_page)
        self.images = lscat.handle_scroll(self._tracker_class, self._data, myslice)

    def scroll_or_show(self):
        if self.scrollable:
            self.handle_scroll()
        else:
            lscat.show_instant(self._tracker_class, self._data)

    def _show_page(self) -> 'IO':
        if not files.dir_not_empty(self._data):
            printer.print_bottom('This is the last page!')
            self._data.page_num -= 1
            return False

        self.scroll_or_show()
        self._report()

    def reload(self) -> 'IO':
        printer.print_bottom(
            'This will delete cached images and redownload them. Proceed?'
        )
        ans = input(f'Directory to be deleted: {self._data.main_path}\n')
        if ans == 'y' or not ans:
            # Will remove all data, but keep info on the main path
            rmtree(self._data.main_path)
            lscat.api.hide_all(self.images)
            self.start(self._data.main_path)
        self._prompt(self)


class AbstractGallery(AbstractUI, ABC):
    @abstractmethod
    def __init__(self, main_path):
        """Complements abstractmethod: Define download function for galleries"""
        self._prompt = prompt.gallery_like_prompt
        self._data_class = data.GalleryData
        self._tracker_class = lscat.TrackDownloads
        self._max_images = utils.max_images()
        self._is_gallery_mode = True
        super().__init__(main_path)

    def _print_page_info(self):
        """Implements abstractmethod: Indicate which posts are multi-image and
        current page number
        """
        printer.print_multiple_imgs(self._data.current_illusts)
        print(f'Page {self._data.page_num}')

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
        """Image mode, from an artist mode (mode 1/5 -> mode 2)"""
        lscat.api.hide_all(self.images)
        idata = Image.from_gallery(self._data, selected_image_num)
        prompt.image_prompt(idata)
        # Image prompt ends, user presses back
        self._back()

    def _back(self) -> 'IO':
        """After user 'back's from image prompt or artist gallery, start mode again"""
        self.scroll_or_show()
        self._report()
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

    def _pixivrequest(self) -> 'IO':
        """Implements abstractmethod: use the user-given id for request"""
        return api.myapi.artist_gallery(self._artist_user_id, self._data.offset)

    def handle_prompt(self, keyseqs):
        """Implements abstractmethod"""
        # Display image (using either coords or image number), the show this prompt
        if keyseqs[0] == 'b':
            lscat.api.hide_all(self.images)
            # Gallery instance stopped here, return to previous state
        elif keyseqs[0] == 'r':
            self.reload()
        elif keyseqs[0].lower() == 'a':
            printer.print_bottom('Invalid command! Press h to show help')
            prompt.gallery_like_prompt(self)  # Go back to while loop

    @staticmethod
    def help() -> 'IO':
        """Implements abstractmethod"""
        printer.print_bottom('')
        printer.print_bottom(
            ''.join(
                colors.base1
                + ['view ']
                + colors.base2
                + ['view ', colors.m, 'anual; ', colors.b, 'ack\n']
            )
        )


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

    def _pixivrequest(self) -> 'IO':
        """Implements abstractmethod, publicity is private for now
        (might be configurable in the future)
        """
        return api.myapi.illust_follow_request(
            restrict='private', offset=self._data.offset
        )

    def go_artist_gallery_coords(self, first_num, second_num: str) -> 'IO':
        """New method for mode 5 only"""
        selected_image_num = utils.find_number_map(int(first_num), int(second_num))
        if selected_image_num is False:  # 0 is valid!
            printer.print_bottom('Invalid number!')
        else:
            self.go_artist_gallery_num(selected_image_num)

    def go_artist_gallery_num(self, selected_image_num: int) -> 'IO':
        """Like self.view_image(), but goes to artist mode instead of image"""
        artist_user_id = self._data.artist_user_id(selected_image_num)
        lscat.api.hide_all(self.images)
        mode = ArtistGallery(artist_user_id)
        prompt.gallery_like_prompt(mode)
        # Gallery prompt ends, user presses back
        self._back()

    def handle_prompt(self, keyseqs):
        """Implements abstractmethod"""
        # "b" must be handled first, because keyseqs might be empty
        if keyseqs[0] == 'b':
            printer.print_bottom('Invalid command! Press h to show help')
            prompt.gallery_like_prompt(self)  # Go back to while loop
        elif keyseqs[0] == 'r':
            self.reload()
        elif keyseqs[0] == 'a':
            self.go_artist_gallery_coords(*keyseqs[-2:])
        elif keyseqs[0] == 'A':
            self.go_artist_gallery_num(pure.concat_seqs_to_int(keyseqs, 1))

    @staticmethod
    def help() -> 'IO':
        """Implements abstractmethod"""
        printer.print_bottom('')
        printer.print_bottom(
            ''.join(
                colors.base1 + [
                    colors.a, "view artist's illusts; ",
                    colors.n, 'ext page;\n',
                    colors.p, 'revious page; ',
                    colors.r, 'eload and re-download all; ',
                    colors.q, 'uit (with confirmation); ',
                    'view ', colors.m, 'anual\n'
                ]
            )
        )


class IllustRelatedGallery(ArtistGallery):
    # Apart from the main_path and the pixivrequest function,
    # everything is the same with mode 1
    __doc__ = ArtistGallery.__doc__.replace('Artist Gallery', 'Related illusts')

    def __init__(self, image_id: int, main_path: 'Path'):
        """Overrides base: pass in image_id (parent dir) & set main_path"""
        self._image_id = image_id
        super().__init__(main_path / str(image_id) / 'illustrelated')

    def _pixivrequest(self) -> 'IO':
        """Overrides base: different method"""
        return api.myapi.illust_related_request(self._image_id, offset=self._data.offset)


class IllustRecommendedGallery(ArtistGallery):
    # Apart from the main_path and the pixivrequest function,
    # everything is the same with mode 1
    # Note that self._data.next_offset seems to be always zero, meaning this is one-page only
    __doc__ = ArtistGallery.__doc__.replace('Artist Gallery', 'Recommended illusts')

    def __init__(self):
        """Overrides base: set main_path"""
        super().__init__(KONEKODIR / 'recommended')

    def _pixivrequest(self) -> 'IO':
        """Overrides base: different method"""
        return api.myapi.illust_recommended_request(offset=self._data.offset)


class AbstractUsers(AbstractUI, ABC):
    """
    User view commands (No need to press enter):
        {n}                -- display illustrations of the nth user
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
        self._prompt = prompt.user_prompt
        self._data_class = data.UserData
        self._tracker_class = lscat.TrackDownloadsUsers
        self._max_images = utils.max_images_user()
        self._is_gallery_mode = False
        super().__init__(main_path)

    def _maybe_join_thread(self):
        """Overrides abstractmethod: Wait for parse_thread to join (if any)"""
        with suppress(AttributeError):
            self.parse_thread.join()

    def _print_page_info(self) -> 'IO':
        """Implements abstractmethod: Indicate current page number"""
        print(f'Page {self._data.page_num}')

    # Unique to Users
    def go_artist_mode(self, selected_user_num: int) -> 'IO':
        """Concrete method unique for both user modes"""
        try:
            artist_user_id = self._data.artist_user_id(selected_user_num)
        except IndexError:
            printer.print_bottom('Invalid number!')
            return False

        lscat.api.hide_all(self.images)
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

    def _pixivrequest(self) -> 'IO':
        return api.myapi.search_user_request(self.user, self._data.offset)


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

    def _pixivrequest(self) -> 'IO':
        return api.myapi.following_user_request(
            self.your_id, self._publicity, self._data.offset
        )


def view_post_mode(image_id) -> 'IO':
    """Image mode, from main (start -> mode 2)"""
    idata = Image.from_main(image_id)
    prompt.image_prompt(idata)


class ToImage(ABC):
    """
    Unlike the other modes, Image does not handle the initial displaying of images
    This is because coming from a gallery mode, the selected image already has a
    square-medium preview downloaded, which can be displayed before the download
    of the large-res completes. Thus, the initial displaying subroutine will be
    different for a standalone mode or coming from a gallery mode.
    """

    def __init__(self):
        self.firstmode: bool

    @abstractmethod
    def get_post_json(self):
        raise NotImplementedError

    @abstractmethod
    def get_image_id(self, post_json: 'Json') -> str:
        raise NotImplementedError

    def maybe_show_preview(self) -> 'Maybe[IO]':
        return True

    @abstractmethod
    def download_image(self, idata) -> 'IO':
        raise NotImplementedError

    def setup(self):
        post_json = self.get_post_json()
        image_id = self.get_image_id(post_json)
        return Image(post_json, image_id, self.firstmode)

    def start(self):
        idata = self.setup()
        self.maybe_show_preview()
        self.download_image(idata)
        idata.display_initial()
        idata.start_preview()
        return idata


class ViewImage(ToImage):
    """Image mode, from an artist mode (mode 1/5 -> mode 2)"""

    def __init__(self, gdata, selected_image_num):
        self._gdata = gdata
        self._selected_image_num = selected_image_num
        self.firstmode = False

    def get_post_json(self):
        return self._gdata.post_json(self._selected_image_num)

    def get_image_id(self, post_json) -> str:
        return post_json.id

    def maybe_show_preview(self) -> 'IO':
        os.system('clear')
        image = sorted(os.listdir(self._gdata.download_path))[self._selected_image_num]
        lscat.api.show_center(self._gdata.main_path / str(self._gdata.page_num) / image)

    def download_image(self, idata) -> 'IO':
        download.download_url(
            idata.download_path, idata.page_urls[0], idata.large_filename
        )


class ViewPostMode(ToImage):
    """Image mode, from main (start -> mode 2)"""

    def __init__(self, image_id):
        self._image_id = image_id
        self.firstmode = True

    def get_post_json(self) -> 'IO':
        printer.print_bottom('Fetching illust details...')
        try:
            return api.myapi.protected_illust_detail(self._image_id)['illust']
        except KeyError:
            printer.print_bottom('Work has been deleted or the ID does not exist!')
            sys.exit(1)

    def get_image_id(self, _) -> str:
        return self._image_id

    def download_image(self, idata) -> 'IO':
        download.download_url(
            idata.download_path, idata.current_url, idata.image_filename
        )


class Image(data.ImageData):  # Extends the data class by adding IO actions on top
    """
    Image view commands (No need to press enter):
        b -- go back to the gallery
        n -- view next image in post (only for posts with multiple pages)
        p -- view previous image in post (only for posts with multiple pages)
        d -- download this image in full resolution
        o -- open this post in browser
        f -- show this image in full resolution
        r -- view related images

        h -- show keybindings
        m -- show this manual
        q -- quit (with confirmation)
    """

    def __init__(self, raw: 'Json', image_id: str, firstmode=False):
        super().__init__(raw, image_id, firstmode)
        # image mode needs to force use_ueberzug=True, see comments in
        # lscat_prompt.ImageLoop
        self.use_ueberzug = True
        self.event = threading.Event()
        # Defined in self.start_preview()
        self.loc: 'tuple[int]'
        self.image: 'list[Image]' = []
        self.preview_images: 'list[Image]' = []

    @classmethod
    def from_gallery(cls, gdata, selected_image_num):
        return ViewImage(gdata, selected_image_num).start()

    @classmethod
    def from_main(cls, image_id):
        return ViewPostMode(image_id).start()

    def display_initial(self) -> 'IO':
        os.system('clear')
        self.image = lscat.api.show_center(self.download_path / self.large_filename)
        printer.print_bottom(
            f'Page 1/{self.number_of_pages}', use_ueberzug=self.use_ueberzug
        )

    def open_image(self) -> 'IO':
        utils.open_in_browser(self.image_id)

    def download_image(self) -> 'IO':
        download.download_url_verified(self.current_original_url)

    def show_full_res(self) -> 'IO':
        filename = pure.split_backslash_last(self.current_original_url)
        download.download_url(self.download_path, self.current_original_url, filename)
        lscat.api.show_center(self.download_path / filename)

    def next_image(self) -> 'IO':
        if not self.page_urls:
            printer.print_bottom(
                'This is the only image in the post!', use_ueberzug=self.use_ueberzug
            )
            return False
        elif self.page_num + 1 == self.number_of_pages:
            printer.print_bottom(
                'This is the last image in the post!', use_ueberzug=self.use_ueberzug
            )
            return False

        # jump_to_image corrects for 1-based
        self.page_num += 1
        self.jump_to_image(self.page_num + 1)

    def previous_image(self) -> 'IO':
        if not self.page_urls:
            printer.print_bottom(
                'This is the only image in the post!', use_ueberzug=self.use_ueberzug
            )
            return False
        elif self.page_num == 0:
            printer.print_bottom(
                'This is the first image in the post!', use_ueberzug=self.use_ueberzug
            )
            return False

        self.page_num -= 1
        self.jump_to_image(self.page_num + 1)

    def jump_to_image(self, selected_image_num: int) -> 'IO':
        self.event.set()
        if selected_image_num <= 0 or selected_image_num > len(self.page_urls):
            printer.print_bottom('Invalid number!', use_ueberzug=self.use_ueberzug)
            return False

        # Internally 0-based, but externally 1-based
        self.page_num = selected_image_num - 1
        self._jump()

    def _jump(self) -> 'IO':
        """Downloads next image if not downloaded, display it, prefetch next"""
        # FIXME: thread might download to the user's current dir.
        # Pass in path to api.download as planned
        threading.Thread(target=self._prefetch_next_image).start()
        if not (self.download_path / self.image_filename).is_dir():
            download.async_download_spinner(self.download_path, [self.current_url])

        os.system('clear')
        lscat.api.hide(self.image)
        lscat.api.hide_all(self.preview_images)

        self.image = lscat.api.show_center(self.filepath)

        printer.print_bottom(
            f'Page {self.page_num+1}/{self.number_of_pages}',
            use_ueberzug=self.use_ueberzug,
        )
        self.start_preview()

    def _prefetch_next_image(self) -> 'IO':
        with suppress(IndexError):
            next_img_url = self.next_img_url
        if next_img_url:
            download.async_download_spinner(self.download_path, [next_img_url])

    def leave(self, force=False) -> 'IO':
        lscat.api.hide(self.image)
        lscat.api.hide_all(self.preview_images)
        self.event.set()
        if self.firstmode or force:
            # Came from view post mode, don't know current page num
            # Defaults to page 1
            mode = ArtistGallery(self.artist_user_id)
            prompt.gallery_like_prompt(mode)
        # Else: image prompt and class ends, goes back to previous mode

    def view_related_images(self):
        lscat.api.hide(self.image)
        lscat.api.hide_all(self.preview_images)
        mode = IllustRelatedGallery(self.image_id, self.download_path.parent)
        prompt.gallery_like_prompt(mode)

    def start_preview(self) -> 'IO':
        self.loc = TERM.get_location()
        if config.api.image_mode_previews() and self.number_of_pages > 1:
            self.event = threading.Event()  # Reset event, in case if it's set
            threading.Thread(target=self.preview).start()

    def preview(self) -> 'IO':
        """Download the next four images in the background and/or display them
        one at a time, so if user interrupts, it won't hang.
        """
        tracker = lscat.TrackDownloadsImage(self)
        i = 1
        while (
            not self.event.is_set()
            and i <= 4
            and self.page_num + i < self.number_of_pages
        ):

            url = self.page_urls[self.page_num + i]
            name = pure.split_backslash_last(url)
            path = self.download_path / name

            if path.is_file():
                tracker.update(name)
            else:
                download.async_download_no_rename(
                    self.download_path, [url], tracker=tracker
                )

            if i == 4:  # Last pic
                printer.move_cursor_xy(self.loc[0], self.loc[1])

            self.preview_images = tracker.images

            i += 1
