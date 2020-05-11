"""Handles user interaction inside all the modes. No knowledge of API needed"""

import os
from abc import ABC, abstractmethod
from pathlib import Path

import funcy
from tqdm import tqdm

from koneko import (KONEKODIR, api, data, main, pure, lscat, utils, colors,
                    prompt, download)


class LastPageException(ValueError):
    pass

class AbstractGallery(ABC):
    def __init__(self, current_page_num):
        self._current_page_num = current_page_num
        self.data: 'data.GalleryJson'
        self._show = True
        # Defined in self.view_image
        self._selected_image_num: int
        # Defined in child classes
        self._main_path: 'Path'
        self._download_path = self._main_path / str(self._current_page_num)

        self.start()

    def start(self):
        """
        If artist_user_id dir exists, show immediately (without checking
        for contents!)
        Else, fetch current_page json and proceed download -> show -> prefetch
        """
        if Path(self._download_path).is_dir():
            try:
                utils.show_artist_illusts(self._download_path)
            except IndexError: # Folder exists but no files
                Path(self._download_path).rmdir()
                self._show = True
            else:
                self._show = False
        else:
            self._show = True

        current_page = self._pixivrequest()
        self.data = data.GalleryJson(current_page)
        download.init_download(self._download_path, self.data,
                               self._current_page_num, download.download_page_pbar,
                               self.data.current_illusts(), self._download_path)

        if self._show:
            utils.show_artist_illusts(self._download_path)

        pure.print_multiple_imgs(self.data.current_illusts())
        print(f'Page {self._current_page_num}')
        # Make sure the following work:
        # Gallery -> next page -> image prompt -> back -> prev page
        if len(self.data.all_pages_cache) == 1:
            # Prefetch the next page on first gallery load
            with funcy.suppress(LastPageException):
                self._prefetch_next_page()


    def open_link_coords(self, first_num, second_num):
        selected_image_num = pure.find_number_map(int(first_num), int(second_num))
        if not selected_image_num:
            print('Invalid number!')
        else:
            self.open_link_num(selected_image_num)

    def open_link_num(self, number):
        # Update current_page_illusts, in case if you're in another page
        image_id = self.data.image_id(self._current_page_num, number)
        link = f'https://www.pixiv.net/artworks/{image_id}'
        os.system(f'xdg-open {link}')
        print(f'Opened {link}!\n')

    def download_image_coords(self, first_num, second_num):
        selected_image_num = pure.find_number_map(int(first_num), int(second_num))
        if not selected_image_num:
            print('Invalid number!')
        else:
            self.download_image_num(selected_image_num)

    def download_image_num(self, number):
        # Update current_page_illusts, in case if you're in another page
        post_json = self.data.post_json(self._current_page_num, number)
        download.download_image_verified(post_json=post_json)

    def view_image(self, selected_image_num):
        self._selected_image_num = selected_image_num
        post_json = self.data.post_json(self._current_page_num, selected_image_num)
        image_id = post_json.id
        idata = data.ImageJson(post_json, image_id)

        display_image(
            post_json,
            idata.artist_user_id,
            self._selected_image_num,
            self._current_page_num
        )

        # blocking: no way to unblock prompt
        image = Image(image_id, idata, self._current_page_num, False)
        prompt.image_prompt(image)

        # Image prompt ends, user presses back
        self._back()

    @abstractmethod
    def _back(self):
        raise NotImplementedError

    def next_page(self):
        download_path = self._main_path / str(self._current_page_num+1)
        try:
            utils.show_artist_illusts(download_path)
        except FileNotFoundError:
            print('This is the last page!')
        else:
            self._current_page_num += 1
            pure.print_multiple_imgs(self.data.current_illusts(self._current_page_num))
            print(f'Page {self._current_page_num}')
            print('Enter a gallery command:\n')

        # Skip prefetching again for cases like next -> prev -> next
        if str(self._current_page_num + 1) not in self.data.cached_pages():
            try:
                # After showing gallery, pre-fetch the next page
                self._prefetch_next_page()
            except LastPageException:
                print('This is the last page!')

    def previous_page(self):
        if self._current_page_num > 1:
            self._current_page_num -= 1

            download_path = (self._main_path / str(self._current_page_num))
            utils.show_artist_illusts(download_path)
            pure.print_multiple_imgs(self.data.current_illusts(self._current_page_num))
            print(f'Page {self._current_page_num}')
            print('Enter a gallery command:\n')

        else:
            print('This is the first page!')

    @abstractmethod
    def _pixivrequest(self, **kwargs):
        raise NotImplementedError

    def _prefetch_next_page(self):
        # TODO: move this somewhere else
        # print("   Prefetching next page...", flush=True, end="\r")
        next_url = self.data.next_url(self._current_page_num)
        if not next_url:  # this is the last page
            raise LastPageException

        parse_page = api.myapi.parse_next(next_url)
        next_page = self._pixivrequest(**parse_page)
        self.data.all_pages_cache[str(self._current_page_num + 1)] = next_page
        current_page_illusts = next_page['illusts']

        download_path = self._main_path / str(self._current_page_num + 1)
        if not Path(download_path).is_dir():
            pbar = tqdm(total=len(current_page_illusts), smoothing=0)
            download.download_page(
                current_page_illusts, download_path, pbar=pbar
            )
            pbar.close()

    def reload(self):
        ans = input('This will delete cached images and redownload them. Proceed?\n')
        if ans == 'y' or not ans:
            os.system(f'rm -r {self._main_path}') # shutil.rmtree is better
            self.data.all_pages_cache = {} # Ensures prefetch after reloading
            self._back()
        else:
            # After reloading, back will return to the same mode again
            prompt.gallery_like_prompt(self)

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
        Using coordinates, where {digit1} is the row and {digit2} is the column
        {digit1}{digit2}   -- display the image on column digit1 and row digit2
        o{digit1}{digit2}  -- open pixiv image/post in browser
        d{digit1}{digit2}  -- download image in large resolution

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
        O9    --->  Open the ninth image's post in browser
        D9    --->  Download the ninth image, in large resolution

        25    --->  Display the image on column 2, row 5 (index starts at 1)
        d25   --->  Open the image on column 2, row 5 (index starts at 1) in browser
        o25   --->  Download the image on column 2, row 5 (index starts at 1)

    """
    def __init__(self, current_page_num, artist_user_id, **kwargs):
        self._main_path = KONEKODIR / str(artist_user_id)
        self._artist_user_id = artist_user_id
        self._kwargs = kwargs
        super().__init__(current_page_num)

    def _pixivrequest(self, **kwargs):
        if kwargs:
            return api.myapi.artist_gallery_parse_next(**kwargs) # Parse next
        else:
            return api.myapi.artist_gallery_request(self._artist_user_id)

    def _back(self):
        # After user 'back's from image prompt, start mode again
        self.__init__(self._current_page_num, self._artist_user_id)
        prompt.gallery_like_prompt(self)

    def handle_prompt(self, keyseqs, gallery_command, selected_image_num,
                      first_num, second_num):
        # Display image (using either coords or image number), the show this prompt
        if gallery_command == 'b':
            pass # Stop gallery instance, return to previous state
        elif gallery_command == 'r':
            self.reload()
        elif keyseqs[0] == 'i':
            self.view_image(selected_image_num)
        elif keyseqs[0].lower() == 'a':
            print('Invalid command! Press h to show help')
            prompt.gallery_like_prompt(self) # Go back to while loop
        elif len(keyseqs) == 2:
            selected_image_num = pure.find_number_map(first_num, second_num)
            if not selected_image_num:
                print('Invalid number!')
                prompt.gallery_like_prompt(self) # Go back to while loop
            else:
                self.view_image(selected_image_num)

    @staticmethod
    def help():
        print(''.join(
            colors.base1 + ['view '] + colors.base2
            + ['view ', colors.m, 'anual; ',
               colors.b, 'ack\n']))


class IllustFollowGallery(AbstractGallery):
    """
    Illust Follow Gallery commands: (No need to press enter)
        Using coordinates, where {digit1} is the row and {digit2} is the column
        {digit1}{digit2}   -- display the image on column digit1 and row digit2
        o{digit1}{digit2}  -- open pixiv image/post in browser
        d{digit1}{digit2}  -- download image in large resolution
        a{digit1}{digit2}  -- view illusts by the artist of the selected image

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
        O9    --->  Open the ninth image's post in browser
        D9    --->  Download the ninth image, in large resolution

        25    --->  Display the image on column 2, row 5 (index starts at 1)
        d25   --->  Open the image on column 2, row 5 (index starts at 1) in browser
        o25   --->  Download the image on column 2, row 5 (index starts at 1)

    """
    def __init__(self, current_page_num):
        self._main_path = KONEKODIR / 'illustfollow'
        super().__init__(current_page_num)

    def _pixivrequest(self, **kwargs):
        if kwargs:
            return api.myapi.illust_follow_request(**kwargs) # Parse next
        else:
            return api.myapi.illust_follow_request(restrict='private') # Publicity

    def go_artist_gallery_coords(self, first_num, second_num):
        selected_image_num = pure.find_number_map(int(first_num), int(second_num))
        if selected_image_num is False: # 0 is valid!
            print('Invalid number!')
        else:
            self.go_artist_gallery_num(selected_image_num)

    def go_artist_gallery_num(self, selected_image_num):
        """Like self.view_image(), but goes to artist mode instead of image"""
        self._selected_image_num = selected_image_num
        post_json = self.data.post_json(self._current_page_num, selected_image_num)

        artist_user_id = post_json['user']['id']
        mode = ArtistGallery(1, artist_user_id)
        prompt.gallery_like_prompt(mode)
        # Gallery prompt ends, user presses back
        self._back()

    def _back(self):
        # User 'back's out of artist gallery, start current mode again
        main.IllustFollowMode(self._current_page_num, self.data)

    def handle_prompt(self, keyseqs, gallery_command, selected_image_num,
                      first_num, second_num):
        # "b" must be handled first, because keyseqs might be empty
        if gallery_command == 'b':
            print('Invalid command! Press h to show help')
            prompt.gallery_like_prompt(self) # Go back to while loop
        elif gallery_command == 'r':
            self.reload()
        elif keyseqs[0] == 'i':
            self.view_image(selected_image_num)
        elif keyseqs[0] == 'a':
            self.go_artist_gallery_coords(first_num, second_num)
        elif keyseqs[0] == 'A':
            self.go_artist_gallery_num(selected_image_num)
        elif len(keyseqs) == 2:
            selected_image_num = pure.find_number_map(first_num, second_num)
            if not selected_image_num:
                print('Invalid number!')
                prompt.gallery_like_prompt(self) # Go back to while loop
            else:
                self.view_image(selected_image_num)

    @staticmethod
    def help():
        print(''.join(colors.base1 + [
            colors.a, "view artist's illusts; ",
            colors.n, 'ext page;\n',
            colors.p, 'revious page; ',
            colors.r, 'eload and re-download all; ',
            colors.q, 'uit (with confirmation); ',
            'view ', colors.m, 'anual\n']))

def display_image(post_json, artist_user_id, number_prefix, current_page_num):
    """
    Opens image given by the number (medium-res), downloads large-res and
    then display that.
    Alternative to main.view_post_mode(). It does its own stuff before calling
    the Image class for the prompt.

    Parameters
    ----------
    number_prefix : int
        The number prefixed in each image
    post_json : JsonDict
    artist_user_id : int
    current_page_num : int
    """
    search_string = f"{str(number_prefix).rjust(3, '0')}_"

    # LSCAT
    os.system('clear')
    arg = KONEKODIR / str(artist_user_id) / str(current_page_num) / search_string / "*"
    os.system(f'kitty +kitten icat --silent {arg}')

    url = pure.url_given_size(post_json, 'large')
    filename = pure.split_backslash_last(url)
    large_dir = KONEKODIR / str(artist_user_id) / str(current_page_num) / "large"
    download.download_core(large_dir, url, filename)

    # BLOCKING: imput is blocking, will not display large image until input
    # received

    # LSCAT
    os.system('clear')
    arg = KONEKODIR / str(artist_user_id) / str(current_page_num) / "large" / filename
    os.system(f'kitty +kitten icat --silent {arg}')


class Image:
    """
    Image view commands (No need to press enter):
        b -- go back to the gallery
        n -- view next image in post (only for posts with multiple pages)
        p -- view previous image in post (same as above)
        d -- download this image
        o -- open pixiv post in browser
        f -- show this image in full resolution

        h -- show keybindings
        m -- show this manual
        q -- quit (with confirmation)

    """
    def __init__(self, image_id, idata, current_page_num=1, firstmode=False):
        self.data = idata
        self._image_id = image_id
        self._current_page_num = current_page_num
        self._firstmode = firstmode

    def open_image(self):
        link = f'https://www.pixiv.net/artworks/{self._image_id}'
        os.system(f'xdg-open {link}')
        print(f'Opened {link} in browser')

    def download_image(self):
        # Doing the same job as full_img_details
        large_url = pure.change_url_to_full(url=self.data.current_url())
        filename = pure.split_backslash_last(large_url)
        filepath = pure.generate_filepath(filename)
        download.download_image_verified(url=large_url, filename=filename,
                                         filepath=filepath)

    def show_full_res(self):
        large_url = pure.change_url_to_full(url=self.data.current_url())
        filename = pure.split_backslash_last(large_url)
        download.download_core(self.data.large_dir, large_url, filename)
        utils.display_image_vp(self.data.large_dir + filename)

    def next_image(self):
        if not self.data.page_urls:
            print('This is the only page in the post!')
        elif self.data.img_post_page_num + 1 == self.data.number_of_pages:
            print('This is the last image in the post!')

        else:
            self.data.img_post_page_num += 1  # Be careful of 0 index
            self._go_next_image()

    def _go_next_image(self):
        """
        Downloads next image if not downloaded, open it, download the next image
        in the background
        """
        # IDEAL: image prompt should not be blocked while downloading
        # But I think delaying the prompt is better than waiting for an image
        # to download when you load it

        # First time from gallery; download next image
        if self.data.img_post_page_num == 1:
            download.async_download_spinner(self.data.large_dir,
                                            [self.data.current_url()])

        utils.display_image_vp(self.data.filepath())

        # Downloads the next image
        try:
            next_img_url = self.data.next_img_url()
        except IndexError: # Last page
            pass
        else:  # No error
            self.data.downloaded_images.append(
                pure.split_backslash_last(next_img_url)
            )
            download.async_download_spinner(self.data.large_dir, [next_img_url])

        print(f'Page {self.data.img_post_page_num+1}/{self.data.number_of_pages}')

    def previous_image(self):
        if not self.data.page_urls:
            print('This is the only page in the post!')
            return False
        elif self.data.img_post_page_num == 0:
            print('This is the first image in the post!')
            return False

        self.data.img_post_page_num -= 1

        testpath = Path(self.data.large_dir) / Path(self.data.image_filename())
        if testpath.is_file():
            utils.display_image_vp(testpath)
        else:
            utils.display_image_vp(
                KONEKODIR / str(self.data.artist_user_id) /
                str(self._current_page_num) / "large" /
                self.data.image_filename()
            )

        print(f'Page {self.data.img_post_page_num+1}/{self.data.number_of_pages}')

    def leave(self, force=False):
        if self._firstmode or force:
            # Came from view post mode, don't know current page num
            # Defaults to page 1
            mode = ArtistGallery(self._current_page_num, self.data.artist_user_id)
            prompt.gallery_like_prompt(mode)
            # After backing
            main.main(start=False)
        # Else: image prompt and class ends, goes back to previous mode


class Users(ABC):
    """
    User view commands (No need to press enter):
        {digit1}{digit2}   -- display artist illusts on column digit1 and row digit2
        n                  -- view next page
        p                  -- view previous page
        r                  -- delete all cached images, re-download and reload view
        h                  -- show keybindings
        m                  -- show this manual
        q                  -- quit (with confirmation)

    """

    @abstractmethod
    def __init__(self, user_or_id):
        # Defined in child classes
        self._main_path: 'Path'
        self._input = user_or_id
        self._offset = 0
        self._page_num = 1
        self.download_path = self._main_path / self._input / str(self._page_num)
        self._show = True
        self.data: 'data.UserJson'

    def start(self):
        # It can't show first (including if cache is outdated),
        # because it needs to print the right message
        # Which means parsing is needed first
        self._parse_and_download()
        if self._show: # Is always true for now
            self._show_page()
        self._prefetch_next_page()

    def _parse_and_download(self):
        """
        Parse info, combine profile pics and previews, download all concurrently,
        move the profile pics to the correct dir (less files to move)
        """
        self._parse_user_infos()
        preview_path = self._main_path / self._input / str(self._page_num) / 'previews'

        download.init_download(self.download_path, self.data,
                               self._page_num, download.user_download,
                               self.data, preview_path, self.download_path,
                               self._page_num)

    @abstractmethod
    def _pixivrequest(self):
        """Blank method, classes that inherit this ABC must override this"""
        raise NotImplementedError

    @pure.spinner('Parsing info...')
    def _parse_user_infos(self):
        """Parse json and get list of artist names, profile pic urls, and id"""
        result = self._pixivrequest()
        if not hasattr(self, 'data'):
            self.data = data.UserJson(result, self._page_num)
        else:
            self.data.update(result, self._page_num)

    def _show_page(self):
        try:
            names = self.data.names(self._page_num)
        except KeyError:
            print('This is the last page!')
            self._page_num -= 1
            self.download_path = self._main_path / self._input / str(self._page_num)

        else:
            names_prefixed = map(pure.prefix_artist_name, names, range(len(names)))
            names_prefixed = list(names_prefixed)

            # LSCAT
            lscat.Card(
                self.download_path,
                self._main_path / self._input / str(self._page_num) / 'previews',
                messages=names_prefixed,
            ).render()

    def _prefetch_next_page(self):
        # TODO: split into download and data parts
        oldnum = self._page_num

        if self.data.next_url:
            next_offset = api.myapi.parse_next(self.data.next_url)['offset']
            # Won't download if not immediately next page, eg
            # p1 (p2 downloaded) -> p2 (p3) -> p1 -> p2 (p4 won't download)
            if int(next_offset) - int(self._offset) <= 30:
                self._offset = next_offset
                self._page_num = int(self._offset) // 30 + 1
                self.download_path = self._main_path / self._input / str(self._page_num)

                self._parse_and_download()

        self._page_num = oldnum
        self.download_path = self._main_path / self._input / str(self._page_num)

    def next_page(self):
        self._page_num += 1
        self.download_path = self._main_path / self._input / str(self._page_num)
        self._show_page()

        self._prefetch_next_page()

    def previous_page(self):
        if self._page_num > 1:
            self._page_num -= 1
            self._offset = int(self._offset) - 30
            self.download_path = self._main_path / self._input / str(self._page_num)
            self._show_page()
        else:
            print('This is the first page!')

    def go_artist_mode(self, selected_user_num):
        try:
            artist_user_id = self.data.artist_user_id(self._page_num, selected_user_num)
        except IndexError:
            print('Invalid number!')
        else:
            mode = ArtistGallery(1, artist_user_id)
            prompt.gallery_like_prompt(mode)
            # After backing from gallery
            self._show_page()
            prompt.user_prompt(self)

    def reload(self):
        ans = input('This will delete cached images and redownload them. Proceed?\n')
        if ans == 'y' or not ans:
            os.system(f'rm -r {self._main_path}') # shutil.rmtree is better
            self.__init__(self._input)
            self.start()
        prompt.user_prompt(self)


class SearchUsers(Users):
    """
    Inherits from Users class, define self._input as the search string (user)
    Parent directory for downloads should go to search/
    """
    def __init__(self, user):
        self._main_path = KONEKODIR / 'search'
        super().__init__(user)

    def _pixivrequest(self):
        return api.myapi.search_user_request(self._input, self._offset)

class FollowingUsers(Users):
    """
    Inherits from Users class, define self._input as the user's pixiv ID
    (Or any other pixiv ID that the user wants to look at their following users)
    Parent directory for downloads should go to following/
    """
    def __init__(self, your_id, publicity='private'):
        self._publicity = publicity
        self._main_path = KONEKODIR / 'following'
        super().__init__(your_id)

    def _pixivrequest(self):
        return api.myapi.following_user_request(self._input, self._publicity, self._offset)
