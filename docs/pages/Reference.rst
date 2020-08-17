Reference
=========

lscat app
---------
.. code-block::

   lscat interactive app

   Usage:
     lscat
     lscat (1|c) [<actions> ...]
     lscat (2|b)
     lscat (3|p) [<path>]
     lscat (4|g)
     lscat (5|u)

   Optional arguments (for specifying a mode):
     1 c  Koneko configuration assistance
     2 b  Browse a cached dir to display
     3 p  Display a specified path
     4 g  Display KONEKODIR / testgallery
     5 u  Display KONEKODIR / testuser

   Possible configuration assistants:
     1  Thumbnail size
     2  x-padding
     3  y-padding
     4  Page spacing
     5  Gallery print spacing
     6  User mode print info x-position
     a  All of the above

.. code-block:: sh

   # No arguments, go to interactive picker
   lscat

   # Mode specified with no extra arguments
   lscat c           # Mode 1: pick configuration assistants to use
   lscat p           # Mode 3: Prompt for a path

   # Mode specified
   lscat c 1 3 4     # Mode 1, go to thumbnail size, y-padding, and page spacing assistants
   lscat b           # Mode 2
   lscat p <path>    # Mode 3, display given path
   lscat g           # Mode 4
   lscat u           # Mode 5


koneko app
----------

.. code-block::

   Browse pixiv in the terminal using kitty's icat to display images (in the
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

.. code-block:: sh

   # No arguments, go to main screen
   koneko

   # No mode specified, can only reach modes a, i, and s:
   koneko https://www.pixiv.net/en/users/2232374         # Mode 1/a
   koneko https://www.pixiv.net/en/artworks/78823485     # Mode 2/i
   koneko "raika9"                                       # Mode 4/s

   # Mode specified
   koneko a 2232374    # Mode 1
   koneko i 78823485   # Mode 2
   koneko f 2232374    # Mode 3
   koneko s "gomzi"    # Mode 4
   koneko n            # Mode 5
   koneko r            # Mode 6


Mode a/1
''''''''

.. code-block::

   Artist Gallery commands: (No need to press enter)
   Using coordinates, where {x} is the row and {y} is the column
       {x}{y}             -- display the image on row {x} and column {y}
       o{x}{y}            -- open pixiv image/post in browser
       d{x}{y}            -- download image in large resolution

   Using image number, where {number} is the nth image in order (see examples)
       i{number}          -- display the image
       O{number}          -- open pixiv image/post in browser.
       D{number}          -- download image in large resolution.

       n                  -- view the next page
       p                  -- view the previous page
       r                  -- delete all cached images, re-download and reload view
       b                  -- go back to previous mode (either 3, 4, 5, or main screen)
       h                  -- show keybindings
       m                  -- show this manual
       q                  -- quit (with confirmation)

   Examples:
       i09   --->  Display the ninth image in image view (must have leading 0)
       i10   --->  Display the tenth image in image view
       O29   --->  Open the last image's post in browser
       D00   --->  Download the first image, in large resolution

       25    --->  Display the image on column 2, row 5 (index starts at 1)
       d25   --->  Open the image on column 2, row 5 (index starts at 1) in browser
       o25   --->  Download the image on column 2, row 5 (index starts at 1)


Mode i/2
''''''''

.. code-block::

   Image view commands (No need to press enter):
       b -- go back to the gallery
       n -- view next image in post (only for posts with multiple pages)
       p -- view previous image in post (only for posts with multiple pages)
       d -- download this image in full resolution
       o -- open this post in browser
       f -- show this image in full resolution
       r -- view related images

       h -- show keybindings
       m -- show this manual
       q -- quit (with confirmation)

Mode f/3 and s/4
''''''''''''''''

.. code-block::

   User view commands (No need to press enter):
       {n}                -- display illustrations of the nth user
       n                  -- view next page
       p                  -- view previous page
       r                  -- delete all cached images, re-download and reload view
       h                  -- show keybindings
       m                  -- show this manual
       q                  -- quit (with confirmation)


Mode n/5
''''''''

.. code-block::

   Illust Follow Gallery commands: (No need to press enter)
   Using coordinates, where {x} is the row and {y} is the column
       {x}{y}             -- display the image on row {x} and column {y}
       o{x}{y}            -- open pixiv image/post in browser
       d{x}{y}            -- download image in large resolution
       a{x}{y}            -- view illusts by the artist of the selected image

   Using image number, where {number} is the nth image in order (see examples)
       i{number}          -- display the image
       O{number}          -- open pixiv image/post in browser.
       D{number}          -- download image in large resolution.
       A{number}          -- view illusts by the artist of the selected image

       n                  -- view the next page
       p                  -- view the previous page
       r                  -- delete all cached images, re-download and reload view
       h                  -- show keybindings
       m                  -- show this manual
       q                  -- quit (with confirmation)

   Examples:
       i09   --->  Display the ninth image in image view (must have leading 0)
       i10   --->  Display the tenth image in image view
       O29   --->  Open the last image's post in browser
       D00   --->  Download the first image, in large resolution

       25    --->  Display the image on column 2, row 5 (index starts at 1)
       d25   --->  Open the image on column 2, row 5 (index starts at 1) in browser
       o25   --->  Download the image on column 2, row 5 (index starts at 1)


Mode r/6
''''''''

See mode a/1.


Related illustrations mode
''''''''''''''''''''''''''

See mode a/1.
