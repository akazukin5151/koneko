"""Browse pixiv in the terminal using kitty's icat to display images (in the
terminal!)

Usage:
  koneko       [<link> | <searchstr>]
  koneko [1|a] <link_or_id>
  koneko [2|i] <link_or_id>
  koneko (3|f) <link_or_id>
  koneko [4|s] <searchstr>
  koneko [5|n]
  koneko (-h | --help)
  koneko (-v | --version)

Notes:
*  If you supply a link and want to go to mode 3, you must give the (3|f) argument,
   otherwise your link would default to mode 1.
*  It is assumed you won't need to search for an artist named '5' or 'n' from the
   command line, because it would go to mode 5.

Optional arguments (for specifying a mode):
  1 a  Mode 1 (Artist gallery)
  2 i  Mode 2 (Image view)
  3 f  Mode 3 (Following artists)
  4 s  Mode 4 (Search for artists)
  5 n  Mode 5 (Newest works from following artists ("illust follow"))

Required arguments if a mode is specified:
  <link>        Pixiv url, auto detect mode. Only works for modes 1, 2, and 4
  <link_or_id>  Either pixiv url or artist ID or image ID
  <searchstr>   String to search for artists

Options:
  (-h | --help)     Show this help
  (-v | --version)  Show version number
"""
# This is needed for testing
import sys

from docopt import docopt

from koneko import pure, __version__


def process_cli_args() -> (str, str):
    """Use docopt to process cli args, returning:
    main_command: string, 1-5
        if user has specified a mode number
    user_input: string
        if user has entered further information required for the mode
        eg, modes 1/5 requires artist user id; mode 2, requires image id
    """
    args = docopt(__doc__)

    # Handle version or help
    if args['--version'] or args['-v']:
        print(__version__)
        return 'vh', ''
    elif args['--help'] or args['-h']:
        print(__doc__)  # Docopt should handle this anyway
        return 'vh', ''

    # Yes it's a lie
    print('Logging in...')

    if (url_or_str := args['<link>']) or (url_or_str := args['<searchstr>']):
        return parse_no_mode(url_or_str)
    return parse_mode_given(args)


def parse_no_mode(url_or_str: str) -> (str, str):
    if 'users' in url_or_str:
        return '1', pure.process_user_url(url_or_str)

    elif 'artworks' in url_or_str or 'illust_id' in url_or_str:
        return '2', pure.process_artwork_url(url_or_str)

    # Assume you won't search for '3' or 'f'
    elif url_or_str == '3' or url_or_str == 'f':
        return '3', ''

    # Assume you won't search for '5' or 'n'
    elif url_or_str == '5' or url_or_str == 'n':
        return '5', ''

    return '4', url_or_str

def parse_mode_given(args: 'dict') -> (str, str):
    url_or_id = args['<link_or_id>']

    if args['1'] or args['a']:
        return '1', pure.process_user_url(url_or_id)

    elif args['2'] or args['i']:
        return '2', pure.process_artwork_url(url_or_id)

    elif args['3'] or args['f']:
        return '3', pure.process_user_url(url_or_id)
    # Mode 4 isn't needed here, because docopt catches <searchstr>

