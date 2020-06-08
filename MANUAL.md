## Conda environment

```sh
conda create -n koneko
conda activate koneko
conda env list                  # make sure you're in the correct environment...
conda install -n koneko pip     # and make sure pip is installed...
which pip                       # and pip is in your conda directory

# Or use manual installation instructions below
# If you're using a separate conda env, you probably want to make some edits
pip install koneko

# Use anywhere (but only in this conda env):
koneko

# To remove the conda env:
conda remove --name koneko --all
```

# Configuration

See [example config](example_config.ini) for reference.

### In general
* Your config must be saved as `~/.config/koneko/config.ini` (exact path and name)
* The credentials section will be automatically generated on first launch
* Gallery means grid: artist illust mode (1) and illust follow mode (5)
* Users (mode) means: following users mode (3) and search users mode (4)

## Print spacing
### number of blank spaces to print before printing numbers/words

* `gallery_print_spacing`: the spacing between column numbers (number of blank spaces between each number)
    * Comma delimited, no spaces
    * Number of values must be equal to the number of columns
* `users_print_name_xcoord`: location of number and artist name, relative from the left side (which should be on the right of the artist profile pic)
* The number(s) are the number of blank spaces (' ') to print
* Both of them act on the x-axis
* These settings are ignored if the `noprint` option is on

## Page spacing
### number of `\n` to print after every page, until all rows are out of view
* `gallery_page_spacing`: for gallery modes (default: 23)
* `users_page_spacing`: for user modes (default: 20)
* For gallery modes, by default one page has two rows of images. The next row cannot be displayed without covering another, so printing newlines will shift the terminal screen down, until the last row is out of view.
* For user modes, each page has one row (each with four pics: the artist profile picture and three previews). The same principle applies: find a value such that the completed four-picture row is completely out of view.
* Both of them act on the y-axis

## Image configuration
* `image_width`: width of the image (default: 18)
* `image_height`: height of the image (default: 8)
* `image_thumbnail_size`: just a value for pixcat (default: 310)
* `images_x_spacing`: horizontal spacing between images (default: 2)
* `images_y_spacing`: vertical spacing between images in one page (default: 1)

## Misc
* `noprint`: Turns off printing the column numbers for the gallery, and number+artist name for user modes.
* Anything not exactly ('1', 'yes', 'true', or 'on') will be considered off

# Usage

```
Browse pixiv in the terminal using kitty's icat to display images (in the
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
```

```
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
```

```
Image view commands (No need to press enter):
    b -- go back to the gallery
    n -- view next image in post (only for posts with multiple pages)
    p -- view previous image in post (only for posts with multiple pages)
    d -- download this image in full resolution
    o -- open this post in browser
    f -- show this image in full resolution

    h -- show keybindings
    m -- show this manual
    q -- quit (with confirmation)
```

```
User view commands (No need to press enter):
    {x}{y}   -- display artist illusts on column {x} and row {y}
    n                  -- view next page
    p                  -- view previous page
    r                  -- delete all cached images, re-download and reload view
    h                  -- show keybindings
    m                  -- show this manual
    q                  -- quit (with confirmation)
```

```
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
    b                  -- go back to main screen
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
```
