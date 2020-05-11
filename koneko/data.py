"""Stores json data from the api. Acts as frontend to access data in a single line.
Functionally pure, no side effects (but stores state)
"""
from koneko import KONEKODIR, pure


class GalleryJson:
    """Stores data for gallery modes (mode 1 and 5)"""
    def __init__(self, raw):
        self.raw = raw
        self.all_pages_cache = {'1': self.raw}

    def current_page(self, current_page_num=1):
        return self.all_pages_cache[str(current_page_num)]

    def current_illusts(self, current_page_num=1):
        return self.all_pages_cache[str(current_page_num)]['illusts']

    def post_json(self, current_page_num, post_number):
        return self.current_illusts(current_page_num)[post_number]

    def image_id(self, current_page_num, number):
        return self.current_illusts(current_page_num)[number]['id']

    def cached_pages(self):
        return self.all_pages_cache.keys()

    def next_url(self, current_page_num):
        return self.all_pages_cache[str(current_page_num)]['next_url']

    def first_img(self):
        return pure.post_titles_in_page(self.current_illusts())[0]


class ImageJson:
    """Stores data for image view (mode 2)"""
    def __init__(self, raw, image_id):
        self._raw = raw
        self.url = pure.url_given_size(self._raw, 'large')
        self.filename = pure.split_backslash_last(self.url)
        self.artist_user_id = self._raw['user']['id']
        self.img_post_page_num = 0

        self.number_of_pages, self.page_urls = pure.page_urls_in_post(self._raw, 'large')
        if self.number_of_pages == 1:
            self.downloaded_images = None
            self.large_dir = KONEKODIR / str(self.artist_user_id) / 'individual'
        else:
            self.downloaded_images = list(map(pure.split_backslash_last,
                                              self.page_urls[:2]))
            # So it won't be duplicated later
            self.large_dir = (KONEKODIR / str(self.artist_user_id) / 'individual' /
                             str(image_id))

    def image_filename(self):
        return self.downloaded_images[self.img_post_page_num]

    def filepath(self):
        return ''.join([self.large_dir, self.image_filename()])

    def next_img_url(self):
        return self.page_urls[self.img_post_page_num + 1]

    def current_url(self):
        return self.page_urls[self.img_post_page_num]


class UserJson:
    """Stores data for user views (modes 3 and 4)"""
    def __init__(self, raw, page_num):
        self.ids_cache, self.names_cache = {}, {}
        self.update(raw, page_num)

    def update(self, raw, page_num):
        self.raw = raw
        self.next_url = self.raw['next_url']
        page = self.raw['user_previews']

        ids = list(map(self._user_id, page))
        self.ids_cache.update({page_num: ids})

        names = list(map(self._user_name, page))
        self.names_cache.update({page_num: names})

        self.profile_pic_urls = list(map(self._user_profile_pic, page))

        # max(i) == number of artists on this page
        # max(j) == 3 == 3 previews for every artist
        self.image_urls = [page[i]['illusts'][j]['image_urls']['square_medium']
                           for i in range(len(page))
                           for j in range(len(page[i]['illusts']))]

    def artist_user_id(self, page_num, selected_user_num):
        return self.ids_cache[page_num][selected_user_num]

    def names(self, page_num):
        return self.names_cache[page_num]

    def all_urls(self):
        return self.profile_pic_urls + self.image_urls

    def all_names(self, page_num):
        preview_names_ext = map(pure.split_backslash_last, self.image_urls)
        preview_names = [x.split('.')[0] for x in preview_names_ext]
        return self.names(page_num) + preview_names

    def splitpoint(self):
        return len(self.profile_pic_urls)

    def first_img(self, page_num=1):
        return self.all_names(page_num)[0]

    @staticmethod
    def _user_id(json):
        return json['user']['id']

    @staticmethod
    def _user_name(json):
        return json['user']['name']

    @staticmethod
    def _user_profile_pic(json):
        return json['user']['profile_image_urls']['medium']
