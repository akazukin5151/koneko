from typing import Callable, Any, TypeVar, Tuple, Union
import time
import json
import socket

# pixivpy added types in feb 17, but the latest stable pypi release is on feb 4
# https://github.com/upbit/pixivpy/pull/206
from pixivpy3 import AppPixivAPI

from internal_types import DownloadInfo, Response
from api import user_illusts, illust_detail, search_user, user_following, illust_follow, illust_recommended, download, login

def logger(label: str, s: Any) -> None:
    with open('log-p', 'a') as f:
        f.write(label + ' ' + str(s) + '\n')

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
    action_info: Any,
    ident: int
) -> str:
    contents: list[Any] = action_info['contents']
    arg = contents[0]
    offset: int = contents[1]
    return f(api, arg, offset, ident)

def handle_request(api: Any, j: Any) -> str:
    action_info: Any = j['action']['contents']
    function: str = action_info['tag']
    ident = j['ident']

    if function == 'user_illusts':
        return handle_action(user_illusts, str, api, action_info, ident)
    elif function == 'illust_detail':
        arg0: str = action_info['contents']
        return illust_detail(api, arg0, ident)
    elif function == 'search_user':
        return handle_action(search_user, str, api, action_info, ident)
    elif function == 'user_following':
        return handle_action(user_following, str, api, action_info, ident)
    elif function == 'illust_follow':
        arg1: int = action_info['contents']
        return illust_follow(api, arg1, ident)
    elif function == 'illust_recommended':
        arg2: int = action_info['contents']
        return illust_recommended(api, arg2, ident)
    response: Response = {
        'ident': ident,
        'response': {
            'tag': 'Error',
            'contents': 'Invalid request in handle_request'
        }
    }
    return json.dumps(response)

def handle_download(api: Any, j: Any, s: socket.socket) -> None:
    download_infos: list[DownloadInfo] = j['action']['contents']
    # TODO: only one identifier for an entire batch of images to download
    ident = j['ident']
    for idx, download_info in enumerate(download_infos):
        path = download(
            api,
            download_info['url'],
            download_info['path'],
            download_info['name']
        )
        response: Response = {
            'ident': ident + idx,
            'response': {'tag': 'Downloaded', 'contents': path}
        }
        res = json.dumps(response)
        with open('logp', 'a') as f:
            f.write(f"{res=}\n")
        try_send(s, res, ident)

def handle(s: socket.socket, api: Any, j: Any) -> None:
    # ReportLen messages won't receive a response, so their ident is ignored
    if j['action']['tag'] == 'ReportLen':
        data = handle_len(s, j)
        jsn = json.loads(data)
        handle_inner(s, api, jsn)
    else:
        handle_inner(s, api, j)

def handle_inner(s: socket.socket, api: Any, j: Any) -> None:
    with open('logp', 'a') as f:
        f.write(f'handle inner {j=}\n')
    ident = j['ident']
    if j['action']['tag'] == 'Login':
        api, res = login(api, j['action']['contents'], ident)
        try_send(s, res, ident)

    elif j['action']['tag'] == 'Request':
        # TODO: login failure should be handled in haskell side
        #if not logged_in:
        #    response = {'response': {'tag': 'Error', 'contents': 'NotLoggedIn'}}
        #    res = json.dumps(response)
        #    return

        res = handle_request(api, j)
        try_send(s, res, ident)

    elif j['action']['tag'] == 'Download':
        handle_download(api, j, s)

def handle_len(s: socket.socket, j: Any) -> bytes:
    total_length: int = j['action']['contents']

    current_length = 0
    result = b''
    while current_length != total_length:
        piece = s.recv(4096)
        messages = piece.split(b'\n')
        for m in messages:
            if m == b'':
                continue
            result += piece
            current_length = len(result)
    return result

def main_() -> None:
    api = AppPixivAPI()
    #logged_in = False
    with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as s:
        s.connect("/tmp/test_sock.ipc")
        while True:
            msg = s.recv(4096)
            if msg == b'':
                continue

            messages = msg.split(b'\n')
            for m in messages:
                if m == b'':
                    continue
                j: Any = json.loads(m)
                handle(s, api, j)

def try_send(s: socket.socket, res: str, ident: int) -> None:
    # length is the length haskell should expect *after* splitting by newlines
    msg = res.encode('utf-8')
    msg_padded = (res + '\n').encode('utf-8')
    header: Response = {
        'ident': ident,
        'response': {'tag': 'ReportLen', 'contents': len(msg)}
    }
    header_msg = json.dumps(header) + '\n'
    s.sendall(header_msg.encode('utf-8'))
    time.sleep(0.1)
    s.sendall(msg_padded)


if __name__ == '__main__':
    main()
