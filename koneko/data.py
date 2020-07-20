"""Stores json data from the api. Acts as frontend to access data in a single line.
Functionally pure, no side effects (but stores state)
Despite GalleryData and UserData having lots of shared attributes/properties/methods,
there isn't much shared functionality, so there's nothing to extract to an abstract
base class
"""
from functools import lru_cache, cached_property
from abc import ABC, abstractmethod

from koneko import pure, KONEKODIR


class AbstractData(ABC):
    @abstractmethod
    def __init__(self):
        # Defined in child classes, can be attribute or property
        self.page_num: int
        self.main_path: 'Path'
        self.offset: int

        self.next_url: str
        self.all_urls: 'list[str]'
        self.all_names: 'list[str]'

    @abstractmethod
    def update(self):
        raise NotImplementedError

    @abstractmethod
    def artist_user_id(self):
        raise NotImplementedError

    @property
    def download_path(self) -> str:
        """Get the download path of the current page"""
        return self.main_path / str(self.page_num)

    @property
    def next_offset(self) -> str:
        return self.next_url.split('&')[-1].split('=')[-1]

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
    def __init__(self, page_num: int, main_path: 'Path'):
        self.page_num = page_num
        self.main_path = main_path
        self.all_pages_cache = {}
        self.offset = 0

    # Required
    def update(self, raw: 'Json'):
        """Adds newly requested raw json into the cache"""
        self.all_pages_cache[str(self.page_num)] = raw

    def artist_user_id(self, post_number: int) -> str:
        """Get the artist user id for a specified post number"""
        return self.post_json(post_number)['user']['id']

    @property
    def next_url(self) -> str:
        return self.all_pages_cache[str(self.page_num)]['next_url']

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
        return self.all_pages_cache[str(self.page_num)]['illusts']

    def post_json(self, post_number: int) -> 'Json':
        """Get the post json for a specified post number"""
        return self.current_illusts[post_number]

    def image_id(self, post_number: int) -> str:
        """Get the image id for a specified specified post number"""
        return self.post_json(post_number)['id']

    def url(self, number: int) -> str:
        return pure.url_given_size(self.post_json(number), 'large')


class UserData(AbstractData):
    def __init__(self, page_num: int, main_path: 'Path'):
        self.page_num = page_num
        self.main_path = main_path
        self.all_pages_cache = {}
        self.offset = 0

    # Required
    def update(self, raw: 'Json'):
        """Adds newly requested raw json into the cache"""
        self.all_pages_cache[self.page_num] = raw['user_previews']
        # This shows the limitations of the current data class design
        # When this class is instantiated, there is basically no data in
        # Only when a fetch occurs, it is updated with the cache and next_url
        # Only then, does the other methods work.
        # It is essentially two classes, one is a fancy tuple of
        # (page_num, main_path, offset);
        # the other class is the cache with all its methods
        # While python will happily allow you to arbitarily add attributes and methods
        # to the class on the fly, perhaps it is time to rethink the design
        self.next_url = raw['next_url']
        # The ImageData class doesn't have this problem because its setup was done
        # by another function -- its ui class was instantiated at the same time as the data

    @lru_cache
    def artist_user_id(self, post_number: int) -> str:
        """Get the artist user id for a specified post number"""
        return self._iterate_cache(lambda x: x['user']['id'])[post_number]


    @cached_property
    def all_urls(self) -> 'list[str]':
        return self.profile_pic_urls + self.image_urls

    @cached_property
    def all_names(self) -> 'list[str]':
        preview_names_ext = map(pure.split_backslash_last, self.image_urls)
        preview_names = [x.split('.')[0] for x in preview_names_ext]
        return self.names + preview_names

    # Unique
    @cached_property
    def names(self) -> 'list[str]':
        return self._iterate_cache(lambda x: x['user']['name'])

    @cached_property
    def profile_pic_urls(self):
        return self._iterate_cache(lambda x: x['user']['profile_image_urls']['medium'])

    @lru_cache
    def _iterate_cache(self, func) -> 'list[str]':
        return [func(x) for x in self.all_pages_cache[self.page_num]]

    @cached_property
    def image_urls(self):
        return [illust['image_urls']['square_medium']
                for post in self.all_pages_cache[self.page_num]
                for illust in post['illusts']]


    @cached_property
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
            self.download_path = self.download_path / str(image_id)

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

