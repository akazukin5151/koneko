"""Stores json data from the api. Acts as frontend to access data in a single line.
Functionally pure, no side effects (but stores state)
"""
from koneko import KONEKODIR, pure


class GalleryJson:
    """Stores data for gallery modes (mode 1 and 5)
    Structure of the JSON raw:
        illust                  (list of posts)         self.current_illusts
            0                   (the first post)        self.first_img & post_json(0)
                id              (image id)              self.image_id
                user
                    id          (artist id)             self.artist_user_id
                    ...
                ...
            1                   (the second post)       self.post_json(1)
                ...
            2                                           self.post_json(2)
                ...
            ...
        next_url                (next JSON url)         self.next_url
    """
    def __init__(self, current_page_num, main_path):
        self.current_page_num = current_page_num
        self._main_path = main_path
        self.all_pages_cache = {}

    def update(self, raw):
        """Adds newly requested raw json into the cache"""
        self.all_pages_cache[str(self.current_page_num)] = raw

    @property
    def current_illusts(self):
        """Get the illusts json for this page"""
        return self.all_pages_cache[str(self.current_page_num)]['illusts']

    @property
    def cached_pages(self):
        return self.all_pages_cache.keys()

    @property
    def next_url(self):
        return self.all_pages_cache[str(self.current_page_num)]['next_url']

    @property
    def download_path(self):
        """Get the download path of the current page"""
        return self._main_path / str(self.current_page_num)

    def post_json(self, post_number):
        """Get the post json for a specified post number"""
        return self.current_illusts[post_number]

    def artist_user_id(self, post_number):
        """Get the artist user id for a specified post number"""
        return self.post_json(post_number)['user']['id']

    def image_id(self, post_number):
        """Get the image id for a specified specified post number"""
        return self.post_json(post_number)['id']

    @property
    def first_img(self):
        return pure.post_titles_in_page(self.current_illusts)[0]

    @property
    def page_num(self):
        """Just a wrapper, for init_download"""
        return self.current_page_num


class ImageJson:
    """Stores data for image view (mode 2)"""
    def __init__(self, raw, image_id):
        self.image_id = image_id
        self.url = pure.url_given_size(raw, 'large')
        self.filename = pure.split_backslash_last(self.url)
        self.artist_user_id = raw['user']['id']
        self.page_num = 0

        self.number_of_pages, self.page_urls = pure.page_urls_in_post(raw, 'large')
        if self.number_of_pages == 1:
            self.downloaded_images = None
            self.large_dir = KONEKODIR / str(self.artist_user_id) / 'individual'
        else:
            self.downloaded_images = list(map(pure.split_backslash_last,
                                              self.page_urls[:2]))
            # So it won't be duplicated later
            self.large_dir = (KONEKODIR / str(self.artist_user_id) / 'individual' /
                              str(image_id))

    @property
    def image_filename(self):
        return self.downloaded_images[self.page_num]

    @property
    def filepath(self):
        return self.large_dir / self.image_filename

    @property
    def next_img_url(self):
        return self.page_urls[self.page_num + 1]

    @property
    def current_url(self):
        return self.page_urls[self.page_num]


class UserJson:
    """Stores data for user views (modes 3 and 4)"""
    def __init__(self, page_num, main_path, user_or_id):
        self.page_num = page_num
        self.main_path = main_path
        self._input = user_or_id
        self.offset = 0

        self.ids_cache, self.names_cache = {}, {}

    @property
    def download_path(self):
        return self.main_path / self._input / str(self.page_num)

    def update(self, raw):
        self.next_url = raw['next_url']
        page = raw['user_previews']

        ids = list(map(self._user_id, page))
        self.ids_cache.update({self.page_num: ids})

        names = list(map(self._user_name, page))
        self.names_cache.update({self.page_num: names})

        self.profile_pic_urls = list(map(self._user_profile_pic, page))

        # max(i) == number of artists on this page
        # max(j) == 3 == 3 previews for every artist
        self.image_urls = [page[i]['illusts'][j]['image_urls']['square_medium']
                           for i in range(len(page))
                           for j in range(len(page[i]['illusts']))]

    def artist_user_id(self, selected_user_num):
        return self.ids_cache[self.page_num][selected_user_num]

    @property
    def all_urls(self):
        return self.profile_pic_urls + self.image_urls

    @property
    def names(self):
        return self.names_cache[self.page_num]

    @property
    def all_names(self):
        preview_names_ext = map(pure.split_backslash_last, self.image_urls)
        preview_names = [x.split('.')[0] for x in preview_names_ext]
        return self.names + preview_names

    @property
    def splitpoint(self):
        """Number of artists. The number where artists stop and previews start"""
        return len(self.profile_pic_urls)

    @property
    def first_img(self):
        return self.all_names[0]

    @staticmethod
    def _user_id(json):
        return json['user']['id']

    @staticmethod
    def _user_name(json):
        return json['user']['name']

    @staticmethod
    def _user_profile_pic(json):
        return json['user']['profile_image_urls']['medium']
