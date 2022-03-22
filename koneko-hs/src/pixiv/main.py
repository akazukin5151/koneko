from typing import cast, Union, Callable, Any
import time
import json
from pathlib import Path
import socket

# pixivpy added types in feb 17, but the latest stable pypi release is on feb 4
# https://github.com/upbit/pixivpy/pull/206
from pixivpy3 import AppPixivAPI

from internal_types import IPCJson, DownloadInfo, PixivRequest
from api import user_illusts, illust_detail, search_user, user_following, illust_follow, illust_recommended, download, login

KONEKO_DIR = Path('~/.local/share/koneko/cache').expanduser()

def main() -> None:
    try:
        main_()
    # Prevent exceptions from being printed because it messes up the haskell UI
    except Exception as e:
        with open('log-p-e', 'a') as f:
            f.write(str(e.__dict__))

def try_parse(data: bytes) -> Union[IPCJson, bool]:
    # TODO: send error response if fails
        return cast(IPCJson, json.loads(data))

def handle_action(
    f: Callable[..., str],
    expected_arg_type: type,
    api: Any,
    action_info: PixivRequest
) -> str:
    contents = action_info['contents']
    if type(contents) == list:
        arg = contents[0]
        offset = contents[1]
        if type(arg) == expected_arg_type and type(offset) == int:
            return f(api, arg, offset)
    return f'{type(contents)=}\n{type(offset)=}\n{type(arg)=}'
    # TODO: send response if type doesn't match

def handle_request(api: Any, j: IPCJson) -> str:
    action_info = cast(PixivRequest, j['action']['contents'])
    function: str = action_info['tag']

    if function == 'user_illusts':
        return handle_action(user_illusts, str, api, action_info)
    elif function == 'illust_detail':
        arg = action_info['contents']
        if type(arg) == str:
            return illust_detail(api, arg)
    elif function == 'search_user':
        return handle_action(search_user, str, api, action_info)
    elif function == 'user_following':
        return handle_action(user_following, str, api, action_info)
    elif function == 'illust_follow':
        arg = action_info['contents']
        if type(arg) == int:
            return illust_follow(api, arg)
    elif function == 'illust_recommended':
        arg = action_info['contents']
        if type(arg) == int:
            return illust_recommended(api, arg)
    return f'{function=}'

def handle_download(api: Any, j: IPCJson, s: socket.socket) -> None:
    download_infos = cast(list[DownloadInfo], j['action']['contents'])
    for download_info in download_infos:
        path = download(
            api,
            download_info['url'],
            download_info['path'],
            download_info['name']
        )
        response = {'response': {'tag': 'Downloaded', 'contents': path}}
        res = json.dumps(response) + '\n'
        try_send(s, res)

def recv_all(s: socket.socket) -> bytes:
    while True:
        header = s.recv(4096)
        if header != b'':
            break
    j: IPCJson = json.loads(header)
    if j['action']['tag'] == 'ReportLen':
        total_length: int = cast(int, j['action']['contents'])

    current_length = 0
    result = b''
    while current_length != total_length:
        piece = s.recv(4096)
        result += piece
        current_length = len(result)
    return result

def main_() -> None:
    api = AppPixivAPI()
    logged_in = False
    with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as s:
        s.connect("/tmp/test_sock.ipc")
        while True:
            data = recv_all(s)
            #with open('log-1', 'a') as f:
            #    f.write(data.decode('utf-8'))

            j = try_parse(data)
            if j is False:
                continue
            j = cast(IPCJson, j)

            if j['action']['tag'] == 'Login':
                refresh_token = cast(str, j['action']['contents'])
                api, res = login(api, refresh_token)
                logged_in = True
                try_send(s, res)

            elif j['action']['tag'] == 'Request':
                if not logged_in:
                    response = {'response': {'tag': 'Error', 'contents': 'NotLoggedIn'}}
                    res = json.dumps(response)
                    continue

                res = handle_request(api, j)
                try_send(s, res)

            elif j['action']['tag'] == 'Download':
                handle_download(api, j, s)

def try_send(s: socket.socket, res: str) -> None:
    msg = res.encode('utf-8')
    header = {'response': {'tag': 'ReportLen', 'contents': len(msg)}}
    header_s = json.dumps(header)
    s.sendall(header_s.encode('utf-8'))
    time.sleep(0.1)
    s.sendall(msg)


if __name__ == '__main__':
    main()
