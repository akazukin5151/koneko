from pathlib import Path

from blessed import Terminal

__version__ = '0.9.2'
KONEKODIR = Path('~/.local/share/koneko/cache').expanduser()
TERM = Terminal()
