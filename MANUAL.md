## Contents

* [Usage](#Usage)
    * [Initial setup](#initial-setup)
    * [Command line usage](#command-line-usage)
    * [Mode a/1](#mode-a1)
    * [Mode i/2](#mode-i2)
    * [Mode f/3 and s/4](#mode-f3-and-s4)
    * [Mode n/5](#mode-n5)
    * [Mode r/6](#mode-r6)
* [Configuration](#Configuration)
    * [In general](#in-general)
    * [Image](#image-configuration)
    * [Print spacing](#print-spacing)
    * [Page spacing](#page-spacing)
    * [Misc](#misc)
    * [Experimental](#experimental)


# Usage

## Initial setup

On your first time, be sure to use the configuration assistant at `lscat 1 7`, and copy + paste the suggestions to your config at `~/.local/config/koneko/config.ini` (exact path)

Launch `koneko`. There are six modes of operation reachable from the main screen. The mode letter is bolded:

1. View **a**rtist illustrations ([ex](https://www.pixiv.net/bookmark.php?type=user))
2. View a post's **i**mages ([ex](https://www.pixiv.net/en/artworks/78823485))
    - View related images suggested by pixiv (ex: scroll down from the above example) (note: only reachable from images view)
3. View the artists that you are **f**ollowing (or any other user ID) ([ex](https://www.pixiv.net/bookmark.php?type=user))
4. **S**earch for an artist/user ([ex](https://www.pixiv.net/search_user.php?nick=raika9&s_mode=s_usr))
5. View **n**ewest illustrations from artists you're following ([ex](https://www.pixiv.net/bookmark_new_illust.php))
6. View **r**ecommended illustrations (now called 'discovery') ([ex](https://www.pixiv.net/discovery))

Enter digits 1-6 to proceed. If prompted, paste in an appropriate pixiv ID or url. See the [command line usage](#command-line-usage) section for url examples.


## Tutorial

* Coordinates are two digits in the form `xy` where x is column and y is row.
    * For example, a command of `25` refers to the item at column 2, row 5
    * Digits are 1-based, starting from the left (x) and the top (y)
    * The first item has coordinates `11`

* Sometimes it's easier to know the index of the post, rather than the coordinates. 
    * Indices are 0-based, two digits, starting from the top left, and row-major (first row from the left to right, then second row from the left to right).
    * **Indices less than 10 need to have a leading zero in the front** (eg third image => `02`)
    * The first item has index `00`

## Mode a/1

![Gallery view_square_medium1](pics/gallery_view_square_medium1.png)

* Press `h` to show a preview of keys
* To view a post in full, enter their coordinates.
* To open a post in your default browser (using `xdg-open`), press `o`, then enter in coordinates as above
* To download the first image of a post in the highest resolution possible to `~/Downloads`, press `d`, then enter in coordinates as above
* Press `i` (for index), then enter in the two digits index as above.
    * For example, `i17` means "view the 18th post".
* Press `n` and `p` to navigate between next and previous pages respectively
* You can open a post or download an image using their indices, by using `O` or `D` respectively (capitalised)
* Press `b` to go back to the main screen. You're done! Please proceed to the [mode i/2](#mode-i2) tutorial

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

## Mode i/2

![Image_view](pics/image_view.png)

* Press `r` to view other images related to this post
    * This mode is functionally identical to [mode a/1](#mode-a1)

```
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
```

## Mode f/3 and s/4

![artist_search](pics/artist_search.png)

* The two digit numbers on top of the user name is the index.
* Enter both digits to view that user's illustrations

```
User view commands (No need to press enter):
    {n}                -- display illustrations of the nth user
    n                  -- view next page
    p                  -- view previous page
    r                  -- delete all cached images, re-download and reload view
    h                  -- show keybindings
    m                  -- show this manual
    q                  -- quit (with confirmation)
```

## Mode n/5

* The only difference between this and mode a/1, is the `a`/`A` command. As usual, the lowercase version is for coordinates, and the uppercase one is for indices
* Entering `a42` means "view the illustrations by the artist of the post at column 4, row 2"
* Entering `A02` means "view the illustrations by the artist of the third post"

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

## Mode r/6

* This mode is functionally identical to [mode a/1](#mode-a1)


## Command line usage

As an alternative to the main screen, you can supply a pixiv url as a command line argument, bypassing the first interactive prompt. The pixiv url must be either the url of the artist's page, or a pixiv post.

Examples
```sh
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
```

Manual
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
```

# Configuration

## It is highly recommended to use the interactive configuration assistant!

After installing, type `lscat 1 7` and follow the instructions. Just copy the suggested settings to your config in `~/.config/koneko/config.ini`. The below text are just for documentation, so don't worry if it is confusing -- it is always better to configure it interactively.

See [example config](example_config.ini) for reference.

### In general
* Your config must be saved as `~/.config/koneko/config.ini` (exact path and name)
* The credentials section will be automatically generated on first launch
* 'Gallery' means grid: artist illust mode (1) and illust follow mode (5)
* 'Users' (mode) means: following users mode (3) and search users mode (4)

## Image configuration
* `image_width`: width of the image, in terms of terminal blank spaces (default: `18`)
* `image_height`: height of the image, in terms of terminal blank spaces (default: `8`)
* `image_thumbnail_size`: size of the image for pixcat (I think it's in pixels) (default: `310`)
* `images_x_spacing`: horizontal spacing between images (default: `2`)
* `images_y_spacing`: vertical spacing between images in a page (default: `1`)

## Print spacing
### Number of blank spaces in between numbers / in front of words

* `gallery_print_spacing`: the spacing between column numbers (number of blank spaces between each number)
    * Integers must be comma delimited, no spaces
    * Number of values must be equal to the number of columns
    * Default: `9,17,17,17,17`
* `users_print_name_xcoord`: x-position of number and artist name, relative from the left side (which should be on the right of the artist profile pic)
    * Default: `18`
* The number(s) are the number of blank spaces (' ') to print
* Both of them act on the x-axis
* These settings are ignored if the `print_info` option is off

## Page spacing
### number of `\n` to print after every page, until all rows are out of view
* `page_spacing`: (see header) 
* Default: `23`
* The next row cannot be displayed without covering another row, so printing newlines will shift the terminal screen down, until the last row is out of view.
* Find a value such that the completed four-picture row is completely out of view.
* Acts on the y-axis

## Misc
* `print_info`: Turns off printing the column numbers for the gallery, and number+artist name for user modes.
* Anything not exactly ('1', 'yes', 'true', or 'on') will be considered off

## Experimental
* `image_mode_previews`: In view post mode (mode 2), preview the next four images for multi-image posts. Unstable because of pixcat implementation details -- it prints out escape codes that moves the terminal cursor, changing the location of other print statements.
