from pathlib import Path
from collections import namedtuple

from blessed import Terminal

__version__ = '0.10.1'
KONEKODIR = Path('~/.local/share/koneko/cache').expanduser()
TERM = Terminal()
FakeData = namedtuple('data', ('download_path',))
