"""Browse pixiv in the terminal using kitty's icat to display images (in the
terminal!)

Usage:
  koneko       [<link> | <searchstr>]
  koneko [1|a] <link_or_id>
  koneko [2|i] <link_or_id>
  koneko (3|f) <link_or_id>
  koneko [4|s] <searchstr>
  koneko [5|n]
  koneko -h

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
  -h  Show this help
"""

import sys
from docopt import docopt
from koneko import pure

def process_cli_args():
    args = docopt(__doc__)
    if len(sys.argv) > 1:
        print('Logging in...')
        prompted = False
    else:  # No cli arguments
        prompted = True
        main_command = None
        user_input = None

    # Direct command line arguments
    if url_or_str := args['<link>']:
        # Link given, no mode specified
        if 'users' in url_or_str:
            user_input, main_command = pure.process_user_url(url_or_str)

        elif 'artworks' in url_or_str or 'illust_id' in url_or_str:
            user_input, main_command = pure.process_artwork_url(url_or_str)

        # Assume you won't search for '3' or 'f'
        elif url_or_str == '3' or url_or_str == 'f':
            main_command = '3'
            user_input = None

        # Assume you won't search for '5' or 'n'
        elif url_or_str == '5' or url_or_str == 'n':
            main_command = '5'
            user_input = None

        else:  # Mode 4, string to search for artists
            user_input = url_or_str
            main_command = '4'

    elif url_or_id := args['<link_or_id>']:
        # Mode specified, argument can be link or id
        if args['1'] or args['a']:
            user_input, main_command = pure.process_user_url(url_or_id)

        elif args['2'] or args['i']:
            user_input, main_command = pure.process_artwork_url(url_or_id)

        elif args['3'] or args['f']:
            user_input, main_command = pure.process_user_url(url_or_id)
            main_command = '3'

    elif user_input := args['<searchstr>']:
        main_command = '4'

    return prompted, main_command, user_input
