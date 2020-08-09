"""Stores json data from the api. Acts as frontend to access data in a single line.
Functionally pure, no side effects (but stores state)
"""
from copy import copy
from functools import lru_cache
from abc import ABC, abstractmethod

from koneko import pure, KONEKODIR


class AbstractData(ABC):
    """
    Note that these (sub)classes methods will fail if update() is not called first.
    When this class is instantiated, there is basically no data inside
    Only when a fetch occurs, it is updated with the cache and next_url
    Only then, does the other methods work.
    It is essentially two classes, one is a fancy tuple of
    (page_num, main_path, offset);
    the other class is the cache with all its methods
    The ImageData class doesn't have this problem because its setup was done
    by another function -- its ui class was instantiated at the same time as the data
    """

    def __init__(self, main_path: 'Path'):
        self.main_path = main_path
        self.offset = 0
        self.page_num = 1
        self.all_pages_cache: 'dict[int, Json]' = {}

        # Must be defined in child classes, can be attribute or property
        self.next_url: str
        self.all_urls: 'list[str]'
        self.all_names: 'list[str]'

    @abstractmethod
    def update(self, raw, page_num=None):
        raise NotImplementedError

    @abstractmethod
    def artist_user_id(self, post_number) -> str:
        raise NotImplementedError

    def clone_with_page(self, page_num):
        new = copy(self)
        new.page_num = page_num
        return new

    @property
    def download_path(self) -> str:
        """Get the download path of the current page"""
        return self.main_path / str(self.page_num)

    @property
    def next_offset(self) -> str:
        return self.next_url.split('&')[-1].split('=')[-1]

    @property
    def is_immediate_next(self) -> bool:
        """
        Prevent download if not immediately next page, eg
        p1 (p2 prefetched) -> p2 (p3) -> p1 -> p2 (p4 won't prefetch)
        """
        return (int(self.next_offset) - int(self.offset)) <= 30

    @property
    def urls_as_names(self) -> 'list[str]':
        return [pure.split_backslash_last(url) for url in self.all_urls]

    @property
    def newnames_with_ext(self) -> 'list[str]':
        return pure.newnames_with_ext(self.all_urls, self.urls_as_names, self.all_names)


class GalleryData(AbstractData):
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

    # Required
    def update(self, raw: 'Json', page_num=None):
        """Adds newly requested raw json into the cache"""
        key = page_num or self.page_num
        self.all_pages_cache[key] = raw

    def artist_user_id(self, post_number: int) -> str:
        """Get the artist user id for a specified post number"""
        return self.post_json(post_number)['user']['id']

    @property
    def next_url(self) -> str:
        return self.all_pages_cache[self.page_num]['next_url']

    @property
    def all_urls(self) -> 'list[str]':
        return pure.medium_urls(self.current_illusts)

    @property
    def all_names(self) -> 'list[str]':
        return pure.post_titles_in_page(self.current_illusts)

    # Unique
    @property
    def current_illusts(self) -> 'Json':
        """Get the illusts json for this page"""
        return self.all_pages_cache[self.page_num]['illusts']

    def post_json(self, post_number: int) -> 'Json':
        """Get the post json for a specified post number"""
        return self.current_illusts[post_number]

    def image_id(self, post_number: int) -> str:
        """Get the image id for a specified specified post number"""
        return self.post_json(post_number)['id']

    def url(self, number: int) -> str:
        return pure.url_given_size(self.post_json(number), 'large')


class UserData(AbstractData):
    # Required
    def update(self, raw: 'Json', page_num=None):
        """Adds newly requested raw json into the cache"""
        key = page_num or self.page_num
        self.all_pages_cache[key] = raw['user_previews']
        self.next_url = raw['next_url']

    @lru_cache
    def artist_user_id(self, post_number: int) -> str:
        """Get the artist user id for a specified post number"""
        return self._iterate_cache(lambda x: x['user']['id'])[post_number]

    @property
    def all_urls(self) -> 'list[str]':
        return self.profile_pic_urls + self.image_urls

    @property
    def all_names(self) -> 'list[str]':
        preview_names_ext = map(pure.split_backslash_last, self.image_urls)
        preview_names = [x.split('.')[0] for x in preview_names_ext]
        return self.names + preview_names

    # Unique
    @property
    def names(self) -> 'list[str]':
        return self._iterate_cache(lambda x: x['user']['name'])

    @property
    def profile_pic_urls(self) -> 'list[str]':
        return self._iterate_cache(lambda x: x['user']['profile_image_urls']['medium'])

    def _iterate_cache(self, func: 'fn(x: Json) -> str') -> 'list[str]':
        return [func(x) for x in self.all_pages_cache[self.page_num]]

    @property
    def image_urls(self) -> 'list[str]':
        return [
            illust['image_urls']['square_medium']
            for post in self.all_pages_cache[self.page_num]
            for illust in post['illusts']
        ]

    @property
    def splitpoint(self) -> int:
        """Number of artists. The number where artists stop and previews start"""
        return len(self.profile_pic_urls)


class ImageData:
    """Stores data for image view (mode 2)"""

    def __init__(self, raw: 'Json', image_id: int, firstmode=False):
        self.image_id = image_id
        self.artist_user_id = raw['user']['id']
        self.page_num = 0
        self.firstmode = firstmode

        # These are assigned here not as a method, as raw won't be updated
        self.page_urls = pure.page_urls_in_post(raw, 'large')
        self.number_of_pages = len(self.page_urls)
        self.download_path = KONEKODIR / str(self.artist_user_id) / 'individual'
        # Store multi image posts within their own dir
        if self.number_of_pages != 1:
            self.download_path /= str(image_id)

    @property
    def current_url(self) -> str:
        return self.page_urls[self.page_num]

    @property
    def next_img_url(self) -> str:
        return self.page_urls[self.page_num + 1]

    @property
    def image_filename(self) -> str:
        return pure.split_backslash_last(self.current_url)

    @property
    def filepath(self) -> str:
        return self.download_path / self.image_filename

    @property
    def large_filename(self) -> str:
        return pure.split_backslash_last(self.page_urls[0])
