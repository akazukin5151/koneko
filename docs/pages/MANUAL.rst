.. _manual:

Manual
======

Usage
-----

Initial setup
^^^^^^^^^^^^^

On your first time, be sure to use the configuration assistant at ``lscat 1 8``\ , and copy + paste the suggestions to your config at ``~/.local/config/koneko/config.ini`` (exact path)

Launch ``koneko``. There are six modes of operation reachable from the main screen. The mode letter is bolded:


#. View **a**\ rtist illustrations (\ `ex <https://www.pixiv.net/bookmark.php?type=user>`_\ )
#. View a post's **i**\ mages (\ `ex <https://www.pixiv.net/en/artworks/78823485>`_\ )

   * View related images suggested by pixiv (ex: scroll down from the above example) (note: only reachable from images view)

#. View the artists that you are **f**\ ollowing (or any other user ID) (\ `ex <https://www.pixiv.net/bookmark.php?type=user>`_\ )
#. **S**\ earch for an artist/user (\ `ex <https://www.pixiv.net/search_user.php?nick=raika9&s_mode=s_usr>`_\ )
#. View **n**\ ewest illustrations from artists you're following (\ `ex <https://www.pixiv.net/bookmark_new_illust.php>`_\ )
#. View **r**\ ecommended illustrations (now called 'discovery') (\ `ex <https://www.pixiv.net/discovery>`_\ )

Enter digits 1-6 to proceed. If prompted, paste in an appropriate pixiv ID or url. See the `command line usage <#command-line-usage>`_ section for url examples.

Tutorial
^^^^^^^^


* 
  Coordinates are two digits in the form ``xy`` where x is column and y is row.


  * For example, a command of ``25`` refers to the item at column 2, row 5
  * Digits are 1-based, starting from the left (x) and the top (y)
  * The first item has coordinates ``11``

* 
  Sometimes it's easier to know the index of the post, rather than the coordinates. 


  * Indices are 0-based, two digits, starting from the top left, and row-major (first row from the left to right, then second row from the left to right).
  * **Indices less than 10 need to have a leading zero in the front** (eg third image => ``02``\ )
  * The first item has index ``00``

Mode a/1
^^^^^^^^


.. image:: ../pics/gallery_view_square_medium1.png
   :target: ../pics/gallery_view_square_medium1.png
   :alt: Gallery view_square_medium1



* Press ``h`` to show a preview of keys
* To view a post in full, enter their coordinates.
* To open a post in your default browser (using ``xdg-open``\ ), press ``o``\ , then enter in coordinates as above
* To download the first image of a post in the highest resolution possible to ``~/Downloads``\ , press ``d``\ , then enter in coordinates as above
* Press ``i`` (for index), then enter in the two digits index as above.

  * For example, ``i17`` means "view the 18th post".

* Press ``n`` and ``p`` to navigate between next and previous pages respectively
* You can open a post or download an image using their indices, by using ``O`` or ``D`` respectively (capitalised)
* Press ``b`` to go back to the main screen. You're done! Please proceed to the `mode i/2 <#mode-i2>`_ tutorial

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
^^^^^^^^


.. image:: ../pics/image_view.png
   :target: ../pics/image_view.png
   :alt: Image_view



* Press ``r`` to view other images related to this post

  * This mode is functionally identical to `mode a/1 <#mode-a1>`_

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
^^^^^^^^^^^^^^^^


.. image:: ../pics/artist_search.png
   :target: ../pics/artist_search.png
   :alt: artist_search



* The two digit numbers on top of the user name is the index.
* Enter both digits to view that user's illustrations

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
^^^^^^^^


* The only difference between this and mode a/1, is the ``a``\ /\ ``A`` command. As usual, the lowercase version is for coordinates, and the uppercase one is for indices
* Entering ``a42`` means "view the illustrations by the artist of the post at column 4, row 2"
* Entering ``A02`` means "view the illustrations by the artist of the third post"

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
^^^^^^^^


* This mode is functionally identical to `mode a/1 <#mode-a1>`_

Command line usage
^^^^^^^^^^^^^^^^^^

As an alternative to the main screen, you can supply a pixiv url as a command line argument, bypassing the first interactive prompt. The pixiv url must be either the url of the artist's page, or a pixiv post.

Examples

.. code-block:: sh

   # No mode specified, can only reach modes a, i, and s:
   koneko https://www.pixiv.net/en/users/2232374         # Mode 1/a
   koneko https://www.pixiv.net/en/artworks/78823485     # Mode 2/i
   koneko "raika9"                                       # Mode 4/s

   # Mode specified
   koneko a 2232374    # Mode 1
   koneko i 78823485   # Mode 2
   koneko f            # Mode 3
   koneko s "gomzi"    # Mode 4
   koneko n            # Mode 5
   koneko r            # Mode 6

Manual

.. code-block::

   Browse pixiv in the terminal using kitty's icat to display images (in the
   terminal!)

   Usage:
     koneko       [<link> | <searchstr>]
     koneko [1|a] <link_or_id>
     koneko [2|i] <link_or_id>
     koneko (3|f)
     koneko [4|s] <searchstr>
     koneko [5|n]
     koneko [6|r]
     koneko [q]
     koneko (-h | --help)
     koneko (-v | --version)

   Notes:
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

lscat app
^^^^^^^^^


#. Configuration assistant: Interactively guides you to setting up your own config.
#. Browse and manage the cache, filter dirs by mode, and view the illustrations/images offline, with similar but reduced functionality
#. Display a specified path. Auto-detects which mode it is, providing that it is a valid dir.
#. Displays the 'testgallery' dir in mode 1, offline. For internal developer use.
#. Displays the 'testuser' dir in mode 3/4, offline. For internal developer use.


* FYI: KONEKODIR is currently set to be ``~/.local/share/koneko/cache``. The parent folder also contains everything else you might want to delete in the even of uninstalling the app
* For developers: simply copy a "page dir" inside a pixiv ID into testgallery (eg, ``cp -r ~/.local/share/koneko/cache/123/1 ~/.local/share/koneko/cache/testgallery``\ ) for mode 4 to work;
* ...and a "page dir" inside 'following' (eg, ``cp -r ~/.local/share/koneko/cache/following/123/1 ~/.local/share/koneko/cache/testuser``\ ) for mode 5 to work.

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

Configuration
-------------

It is highly recommended to use the interactive configuration assistant!
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

After installing, type ``lscat 1 7`` and follow the instructions. Just copy the suggested settings to your config in ``~/.config/koneko/config.ini``. The below text are just for documentation, so don't worry if it is confusing -- it is always better to configure it interactively.

See `example config <example_config.ini>`_ for reference.

In general
~~~~~~~~~~


* Your config must be saved as ``~/.config/koneko/config.ini`` (exact path and name)
* 'Gallery' means grid: artist illust mode (a/1), illust follow mode (n/5), illust recommended mode (r/6), and illust related mode
* 'Users' (mode) means: following users mode (3) and search users mode (4)
* For booleans, ('1', 'yes', 'true', 'on') will be considered True, while ('0', 'no', 'false', 'off') will be considered False

[Credentials]
^^^^^^^^^^^^^


* The credentials section will be automatically generated on first launch


.. list-table::
   :header-rows: 1

   * - Setting
     - Type
     - Default
     - Description
     - Notes
   * - ``refresh_token``
     - string
     -
     - Your pixiv refresh token
     -
   * - ``id``
     - int
     -
     - Your pixiv ID number
     - Optional


[lscat]
^^^^^^^

Image configuration
~~~~~~~~~~~~~~~~~~~


.. list-table::
   :header-rows: 1

   * - Setting
     - Type
     - Default
     - Description
     - Notes
   * - ``image_width``
     - int
     - 18
     - Width of the image, in terms of terminal blank spaces
     -
   * - ``image_height``
     - int
     - 8
     - Height of the image, in terms of terminal blank spaces
     -
   * - ``thumbnail_size``
     - int
     - 310
     - Size of the image for pixcat (I think it's in pixels)
     -
   * - ``images_x_spacing``
     - int
     - 2
     - Horizontal spacing between images in a page
     -
   * - ``images_y_spacing``
     - int
     - 1
     - Vertical spacing between images in a page
     -


Print spacing
~~~~~~~~~~~~~

.. list-table::
   :header-rows: 1

   * - Setting
     - Type
     - Default
     - Description
     - Notes
   * - ``gallery_print_spacing``
     - list[int]
     - 9,17,17,17,17
     - The number of blank spaces between column numbers (number of blank spaces between each number)
     - * Integers must be comma delimited, no spaces in between
       * Number of values must be equal to the number of columns
   * - ``users_print_name_xcoord``
     - int
     - 18
     - The number of blank spaces between the left edge and the artist number and name
     - * x-position of number and artist name, relative from the left side (which should be on the right of the artist profile pic)
       * Number of values must be equal to the number of columns


* Both of them act on the x-axis
* These settings are ignored if the ``print_info`` option is off

Page spacing
~~~~~~~~~~~~

.. list-table::
   :header-rows: 1

   * - Setting
     - Type
     - Default
     - Description
     - Notes
   * - ``page_spacing``
     - int
     - 23
     - The number of ``\n`` to print after every page, until all rows are out of view
     - * Find a value such that the completed four-picture row is completely out of view.
       * Acts on the y-axis



[welcome_screen]
^^^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Setting
     - Type
     - Default
     - Description
     - Notes
   * - ``spaces_to_offset``
     - int
     - 30
     - Number of spaces to pad on the left, to shift the text in the welcome screen to the right of the welcome image
     -
   * - ``image_size``
     - int
     - 600
     - The size of the welcome image
     -



[misc]
^^^^^^

.. list-table::
   :header-rows: 1

   * - Setting
     - Type
     - Default
     - Description
     - Notes
   * - ``print_info``
     - bool
     - on
     - Whether to print the column numbers for gallery modes, and number+artist name for user modes.
     -


[experimental]
^^^^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Setting
     - Type
     - Default
     - Description
     - Notes
   * - ``image_mode_previews``
     - bool
     - off
     - Whether to preview the next four images for multi-image posts, in view post mode (mode i/2)
     - Unstable because of pixcat implementation details -- it prints out escape codes that moves the terminal cursor, changing the location of other print statements.


Ueberzug
~~~~~~~~

(Note: these settings still belong inside the ``[experimental]`` section)

.. list-table::
   :header-rows: 1

   * - Setting
     - Type
     - Default
     - Description
     - Notes
   * - ``use_ueberzug``
     - bool
     - off
     - Whether to use `Ueberzug <https://github.com/seebye/ueberzug>`_ instead of `pixcat <https://github.com/mirukana/pixcat>`_ / kitty's icat.
     -
   * - ``scroll_display``
     - bool
     - on
     - Whether lscat should print newlines to scroll down the terminal and display more images
     - * The number of images in a terminal page is number_of_cols * number_of_rows
       * As the total number of images usually exceed that, lscat will print newlines to offset the old images, so that all images can be displayed. This is what "display scrolling" means
       * The caveat is that the user has to manually scroll with the mouse or the clunky ctrl+shift+up/down
       * If ueberzug is on, this option will always be off, because only pixcat/icat respond to terminal scroll events
   * - ``ueberzug_center_spaces``
     - int
     - 20
     - The x-coordinate of an image that is in the center of your terminal
     -

