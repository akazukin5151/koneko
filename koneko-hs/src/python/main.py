from typing import Callable, Any
import time
import json
import socket

# pixivpy added types in feb 17, but the latest stable pypi release is on feb 4
# https://github.com/upbit/pixivpy/pull/206
from pixivpy3 import AppPixivAPI

from internal_types import DownloadInfo
from api import user_illusts, illust_detail, search_user, user_following, illust_follow, illust_recommended, download, login

def main() -> None:
    try:
        main_()
    # Prevent exceptions from being printed because it messes up the haskell UI
    except Exception as e:
        with open('log-p-e', 'a') as f:
            f.write(str(e.__dict__))

def handle_action(
    f: Callable[..., str],
    expected_arg_type: type,
    api: Any,
    action_info: Any
) -> str:
    contents: list[Any] = action_info['contents']
    arg = contents[0]
    offset: int = contents[1]
    return f(api, arg, offset)

def handle_request(api: Any, j: Any) -> str:
    action_info: Any = j['action']['contents']
    function: str = action_info['tag']

    if function == 'user_illusts':
        return handle_action(user_illusts, str, api, action_info)
    elif function == 'illust_detail':
        arg0: str = action_info['contents']
        return illust_detail(api, arg0)
    elif function == 'search_user':
        return handle_action(search_user, str, api, action_info)
    elif function == 'user_following':
        return handle_action(user_following, str, api, action_info)
    elif function == 'illust_follow':
        arg1: int = action_info['contents']
        return illust_follow(api, arg1)
    elif function == 'illust_recommended':
        arg2: int = action_info['contents']
        return illust_recommended(api, arg2)
    response = {
        'response': {
            'tag': 'Error',
            'contents': 'Invalid request in handle_request'
        }
    }
    return json.dumps(response)

def handle_download(api: Any, j: Any, s: socket.socket) -> None:
    download_infos: list[DownloadInfo] = j['action']['contents']
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
    j: Any = json.loads(header)
    if j['action']['tag'] == 'ReportLen':
        total_length: int = j['action']['contents']

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

            j: Any = json.loads(data)

            if j['action']['tag'] == 'Login':
                api, res = login(api, j['action']['contents'])
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
