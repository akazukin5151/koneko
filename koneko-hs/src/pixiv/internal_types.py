from typing import Optional, Union, Literal, TypedDict

# TODO use enums rather than union of all possible fields
# input types
class PixivRequest(TypedDict):
    tag: Literal[
        'user_illusts',  # str, int
        'illust_detail',  # str
        'search_user',  # str, int
        'user_following',  # str, int
        'illust_follow',  # int
        'illust_recommended',  # int
    ]
    contents: Union[int, str, list[Union[int, str]]]

class DownloadInfo(TypedDict):
    url: str
    path: str
    name: str

class IPCActions(TypedDict):
    tag: Literal[
        'Login',  # str
        'Shutdown',
        'Request',  # PixivRequest
        'Download',  # [DownloadInfo]
        'ReportLen',  # int
    ]
    contents: Union[PixivRequest, list[DownloadInfo], str, int]

class IPCJson(TypedDict):
    action: IPCActions


# output types
PythonError = Literal['NotLoggedIn', 'OtherError']

class Responses(TypedDict):
    tag: Literal[
        'Requested',  # str
        'LoginInfo',  # str
        'Exit',
        'Error',  # PythonError
        'Downloaded',  # str
        'ReportLen'  # int
    ]
    contents: Optional[Union[int, PythonError, str]]

class Response(TypedDict):
    response: Responses

