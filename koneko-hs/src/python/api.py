"""pixiv api functions"""

from typing import Any, Tuple
import json
from pathlib import Path
from internal_types import Response

from pixivpy3 import PixivError
import funcy

def login(api: Any, refresh_token: str) -> Tuple[Any, str]:
    r = api.auth(refresh_token=refresh_token)
    response: Response = {'response': {'tag': 'LoginInfo', 'contents': r}}
    return (api, json.dumps(response))

def user_illusts(api: Any, user_id: str, offset: int) -> str:
    r = api.user_illusts(user_id, offset=offset)
    response: Response = {'response': {'tag': 'Requested', 'contents': json.dumps(r)}}
    return (json.dumps(response))

def illust_detail(api: Any, image_id: str) -> str:
    r = api.illust_detail(image_id)
    response: Response = {'response': {'tag': 'Requested', 'contents': json.dumps(r)}}
    return (json.dumps(response))

def search_user(api: Any, query: str, offset: int) -> str:
    r = api.search_user(query, offset=offset)
    response: Response = {'response': {'tag': 'Requested', 'contents': json.dumps(r)}}
    return (json.dumps(response))

def user_following(api: Any, user_id: str, offset: int) -> str:
    r = api.user_following(user_id, offset=offset, restrict='private')
    response: Response = {'response': {'tag': 'Requested', 'contents': json.dumps(r)}}
    return (json.dumps(response))

def illust_follow(api: Any, offset: int) -> str:
    r = api.illust_follow(restrict='private', offset=offset)
    response: Response = {'response': {'tag': 'Requested', 'contents': json.dumps(r)}}
    return (json.dumps(response))

def illust_recommended(api: Any, offset: int) -> str:
    r = api.illust_recommended(offset=offset)
    response: Response = {'response': {'tag': 'Requested', 'contents': json.dumps(r)}}
    return json.dumps(response)

@funcy.retry(tries=3, errors=(ConnectionError, PixivError))
def download(api: Any, url: str, path: str, name: str) -> str:
    #with open('log-p', 'a') as f:
    #    f.write(f'api.download({url=}, {path=}, {name=})\n')
    api.download(url, path=path, name=name)
    r = str(Path(path) / name)
    return r
