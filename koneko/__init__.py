from pathlib import Path
from collections import namedtuple

from blessed import Terminal

__version__ = '0.11.0'
KONEKODIR = Path('~/.local/share/koneko/cache').expanduser()
WELCOME_IMAGE = KONEKODIR.parent / 'pics' / '71471144_p0.png'
TERM = Terminal()
FakeData = namedtuple('data', ('download_path',))
