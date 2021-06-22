"""Protocol handler for pixiv:// links"""
import os
import sys
from pathlib import Path
from configparser import ConfigParser

from plyer import notification

from koneko.url_login import script


def parse(url: str) -> str:
    #format: 'pixiv://account/login?code=xxxx&via=login'
    remove_head = url.split('code=')[1]
    remove_tail = remove_head.split('&')
    return remove_tail[0]

def get_verifier(remove=True):
    path = Path('~/.local/share/koneko/code_verifier').expanduser()
    with open(path, 'r') as f:
        res = f.read().replace('\n', '')
    if remove:
        path.unlink()
    return res

def write_to_config(token: str) -> 'IO ()':
    parser = ConfigParser()
    parser.read_dict({'Credentials': {'refresh_token': token}})
    config_path = Path('~/.config/koneko/config.ini').expanduser()
    config_path.parent.mkdir(exist_ok=True)
    config_path.touch()
    with open(config_path, 'w') as c:
        parser.write(c)
    append_default_config(config_path)

def append_default_config(config_path) -> 'IO':
    # Why not use python? Because it's functional, readable, and
    # this one liner defeats any potential speed benefits
    example_cfg = Path('~/.local/share/koneko/example_config.ini').expanduser()
    os.system(f'tail {example_cfg} -n +8 >> {config_path}')

def main():
    try:
        url = sys.argv[1]
        code = parse(url)
        code_verifier = get_verifier(remove=True)
        _, token, _ = script.login(code, code_verifier)
        write_to_config(token)
        os.system('rm ~/.local/share/applications/pixiv-url.desktop')
        notification.notify(title='Login finished!', message='Please run koneko again')
    except:
        # Bare except to prevent xdg-open from opening the url
        # containing the sensitive code
        # Logging the exception could be useful for debugging
        notification.notify(title='Login failed!', message='Please submit a bug report')


if __name__ == '__main__':
    main()
