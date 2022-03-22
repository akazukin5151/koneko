from typing import Optional, Union, Literal, TypedDict

# input types
class DownloadInfo(TypedDict):
    url: str
    path: str
    name: str


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

