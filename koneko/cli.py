"""Browse pixiv in the terminal using kitty's icat to display images (in the
terminal!)

Usage:
  koneko       [<link> | <searchstr>]
  koneko [1|a] <link_or_id>
  koneko [2|i] <link_or_id>
  koneko (3|f) <link_or_id>
  koneko [4|s] <searchstr>
  koneko [5|n]
  koneko [6|r]
  koneko [q]
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
  6 r  Mode 6 (Recommended illustrations)

Required arguments if a mode is specified:
  <link>        Pixiv url, auto detect mode. Only works for modes 1, 2, and 4
  <link_or_id>  Either pixiv url or artist ID or image ID
  <searchstr>   String to search for artists

Options:
  (-h | --help)     Show this help
  (-v | --version)  Show version number
"""
import sys  # Needed for tests

from docopt import docopt

from koneko import pure, main, __version__


def handle_vh() -> 'Optional[dict]':
    """Intercepts args, determine whether to quit on help/version"""
    args = docopt(__doc__)

    if args['--version'] or args['-v']:
        print(__version__)
        return False
    elif args['--help'] or args['-h']:
        print(__doc__)  # Docopt should handle this anyway
        return False
    return args


def launch_mode(args, your_id):
    print('Logging in...')

    if (url_or_str := args['<link>']):
        return parse_no_mode(url_or_str, your_id)

    elif (search := args['<searchstr>']):
        return main.SearchUsersModeLoop(search).start()

    return parse_mode_given(args)


def parse_no_mode(url_or_str: str, your_id):
    if 'users' in url_or_str:
        return main.ArtistModeLoop(pure.process_user_url(url_or_str)).start()

    elif 'artworks' in url_or_str or 'illust_id' in url_or_str:
        return main.ViewPostModeLoop(pure.process_artwork_url(url_or_str)).start()

    # Assume you won't search for '3' or 'f'
    elif url_or_str == '3' or url_or_str == 'f':
        return main.FollowingUserModeLoop(your_id).start(your_id)

    # Assume you won't search for '5' or 'n'
    elif url_or_str == '5' or url_or_str == 'n':
        return main.illust_follow_mode()

    # Assume you won't search for '6' or 'r'
    elif url_or_str == '6' or url_or_str == 'r':
        return main.illust_recommended_mode()

    elif url_or_str == 'q':
        return main.frequent()

    return main.SearchUsersModeLoop(url_or_str).start()


def parse_mode_given(args: 'dict') -> (str, str):
    url_or_id = args['<link_or_id>']

    if args['1'] or args['a']:
        return main.ArtistModeLoop(pure.process_user_url(url_or_id)).start()

    elif args['2'] or args['i']:
        return main.ViewPostModeLoop(pure.process_artwork_url(url_or_id)).start()

    elif args['3'] or args['f']:
        return main.FollowingUserModeLoop(pure.process_user_url(url_or_id)).start()

    # Mode 4 isn't needed here, because docopt catches <searchstr>

