# koneko [![GPLv3 license](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.txt) [![PyPI](https://img.shields.io/pypi/v/koneko)](https://pypi.org/project/koneko/) ![build](https://github.com/twenty5151/koneko/workflows/build/badge.svg) [![commits since](https://img.shields.io/github/commits-since/twenty5151/koneko/latest)](https://GitHub.com/twenty5151/koneko/commit/)

Browse pixiv in the terminal using kitty's icat to display images (in the terminal!)

Gallery view
![Gallery view_square_medium1](pics/gallery_view_square_medium1.png)
![Gallery view_square_medium2](pics/gallery_view_square_medium2.png)
Image view
![Image_view](pics/image_view.png)
Artist search (artist profile picture on the left, 3 previews on right)
![artist_search](pics/artist_search.png)
View artists you're following
![following_users_view](pics/following_users_view.png)

Requires [kitty](https://github.com/kovidgoyal/kitty) on Linux. It uses the magical `kitty +kitten icat` 'kitten' to display images. For more info see the [kitty documentation](https://sw.kovidgoyal.net/kitty/kittens/icat.html). Actually, `lscat.py` uses [pixcat](https://github.com/mirukana/pixcat), which is a Python API for icat.

**Why the name Koneko?** Koneko (こねこ) means kitten, which is what `icat` is, a kitty `+kitten`


# Features
See the [manual](#manual) for more details

1. Artist illustrations gallery ([ex](https://www.pixiv.net/bookmark.php?type=user))
    * Enter the post's coordinates to open it in image view. Coordinates are in the form `xy` where x is column and y is row.
    * Next and previous pages
2. Image view ([ex](https://www.pixiv.net/en/artworks/78823485))
    * View an image in large resolution
    * Browse through different images in a multi-image post.
3. View artists you are following ([ex](https://www.pixiv.net/bookmark.php?type=user))
4. Search for an artist ([ex](https://www.pixiv.net/search_user.php?nick=raika9&s_mode=s_usr))
5. View new illustrations from all the artists you are following ([ex](https://www.pixiv.net/bookmark_new_illust.php))
* Both gallery and image views can:
    * Download an image([PixivUtil](https://github.com/Nandaka/PixivUtil2/) would be more suitable for batch download) in full resolution
    * Open post in browser


# Rationale
* Terminal user interfaces are minimalist, fast, and doesn't load Javascript that slows down your entire browser or track you
    * Image loading is *so* much faster, especially if you don't delete the cache

I get 32 trackers on Pixiv. Plus, you have to disable ublock if you ever get logged out

<a href="url"><img src="pics/pixiv_ublock.png" height="350"></a>

The mobile app even directly tells you Google "and our 198 partners" "collect and use data"! See [prompt 1](https://raw.githubusercontent.com/twenty5151/koneko/master/pics/ads1.png), [prompt 2](https://raw.githubusercontent.com/twenty5151/koneko/master/pics/ads2.png) (Github can't render the images correctly for some reason) and this [list](#trackers)

* TUIs make you cool
* TUIs *with embedded pictures* make you even cooler
* TUIs embedded with pictures of cute anime girls make you the coolest
* Keyboard driven
* Familiar, vim-like key sequences
* I use arch btw


# Installation
See also: [manual installation](#manual-installation)

0. Install [kitty](https://github.com/kovidgoyal/kitty)
1. `pip install koneko` (or if you use [conda](#conda)...):
2. Run `koneko`

If it crashes (it shouldn't), it might be because pip didn't 'install' the welcome pictures, *and* the script failed to download them for some reason. Try:

```sh
mkdir -p ~/.local/share/koneko/pics
curl -s https://raw.githubusercontent.com/twenty5151/koneko/master/pics/71471144_p0.png -o ~/.local/share/koneko/pics/71471144_p0.png
curl -s https://raw.githubusercontent.com/twenty5151/koneko/master/pics/79494300_p0.png -o ~/.local/share/koneko/pics/79494300_p0.png
```

# Usage
There are five modes of operation:
1. View artist illustrations ([ex](https://www.pixiv.net/bookmark.php?type=user))
2. View a post ([ex](https://www.pixiv.net/en/artworks/78823485))
3. View the artists that you are following (or any other user ID) ([ex](https://www.pixiv.net/bookmark.php?type=user))
4. Search for artist/user ([ex](https://www.pixiv.net/search_user.php?nick=raika9&s_mode=s_usr))
5. View newest illustrations from artists you're following ([ex](https://www.pixiv.net/bookmark_new_illust.php))

Enter digits 1-5 to proceed. If prompted, paste in an appropriate pixiv ID or url. See below for url examples.

Alternatively, you can supply a pixiv url as a command line argument, bypassing the first interactive prompt. The pixiv url must be either the url of the artist's page, or a pixiv post. Example:

```sh
koneko https://www.pixiv.net/en/users/2232374 # Mode 1
koneko https://www.pixiv.net/en/artworks/78823485 # Mode 2
koneko f https://www.pixiv.net/en/users/2232374 # Mode 3
koneko "raika9" # Mode 4
koneko 5 # Mode 5
```
For more details refer to the [manual](#manual).

# Roadmap

* Complete unit tests
* Startup time seems to be slow, but the delay is before the first line even executes. Import time is fast. `pip install` using the wheel seems to be faster.

## Terminal reliability
* Be responsive to terminal sizes, calculate number of columns and stuff, rather than hardcoding it. (1/2)
* Option to use pillow or wand to edit numbers on pics
* Support [ueberzug](https://github.com/seebye/ueberzug)

## Features

* For multi-image posts in image view, enter a number to jump to the post's page
* Image view should preview the next few images in multi-image posts (currently experimental feature for first image)

## Upcoming changelog (in dev branch)

For full changelogs please see [releases](https://github.com/twenty5151/koneko/releases)

### Version 0.6
* **Depreciated** legacy lscat (the shell script) and lsix support.

#### Features
* If there are cached images in the users modes (3 & 4), it will be shown immediately before requesting and parsing.
* Gallery and User modes (all modes except mode 2) now prefetch the next page in another thread/in the background, freeing up the prompt to respond to user key presses.
* If the cache dir has images, it will display the images before waiting for login, making it much faster.
* Improved experience of invalid input: it will no longer reload to main after 2 seconds.
* Fixed bug in Image mode where the first picture of a multi-picture post prints the current page indicator before the picture is displayed

#### Terminal reliability
* lscat now calculates params using terminal size
* Add option to turn off printing messages in lscat

#### Code
* Removed the `raw` attribute from data classes
* Moved view_post_mode() from main.py to ui.py
* Renamed img_post_page_num to page_num
* Updated unit tests and added more tests
* Revamped lscat to use only the generators to display the images, rather than for loops, heavy classes, and fragile inheritance
    * Users view actually got faster, because two yield statements are apparently faster than two nested for loops
    * No significant speed difference for Galley views.
* Continuous integration tested on linux and macos


# FAQ
* Pixiv keeps emailing me saying I've logged in, every time I use this app!

That's because cookies aren't stored so you log in everytime with a new session. Looking at [PixivUtil's cookie implementation](https://github.com/Nandaka/PixivUtil2/blob/master/PixivBrowserFactory.py), it would be easier to base this app on PixivUtil for downloads, than to write it myself (currently, it's based on the [pixivpy](https://github.com/upbit/pixivpy/) api). The problems with this, other than being a huge time and effort investment, is that koneko uses info from the requests, such as number of pages.
I'd like to fix this but I'd rather not use mechanize but I don't know how to do it either way.

* What operating systems does it support?

It supports all OSes that kitty supports, which means Linux and macOS. It should work on macOS, but I don't have a test device. If you do, please contribute!

* What versions of kitty does it support?

It has been successfuly tested on version 0.17.2 onwards

* Why use threading and not asyncio? There's a async version of pixivpy.

If only I can understand how to use asyncio. See [I don't understand Python's Asyncio](https://lucumr.pocoo.org/2016/10/30/i-dont-understand-asyncio/). I only need to do two simple things. Start this function in the background, wait for its result somewhere later, in another function. Then keep the result so I can use it later. Asyncio is impossible to use.

* I'm having problems with lscat

First, koneko is intended to work for full screen terminals, so don't tile it around unless your screen is big enough. Moving and resizing it abruptly will not be good for icat, which is really kitty's problem not mine.

You can also use versions less than v0.5.1, which retains legacy support for the original lsix shell script. Note that I've never really tested it, which is why I decided to be honest and depreciated legacy support from v0.6 onwards.

# Contributing
* Fork it
* Edit the files on your fork/branch
    * If your git client complains about committing to master, just remove `.pre-commit-config.yaml`
* Run tests with `pytest testing/ -vvvv -l -s --icat`
* Try it with `python koneko/main.py`, or `python setup.py install` then `koneko` to simulate a pip install (or `pip install .`; check out [manual installation](#manual-installation))
    * If doing the latter, make sure you aren't running the released version on pypi (totally didn't happen to me).
* Submit a pull request
* If you want to, you can create an issue first. Ask any questions by opening a new issue.
* If you're encountering/fixing a bug and you're stuck, try clearing the cache. For example, a bug might have downloaded to the wrong folder, but after fixing the bug, you need to clear the cache, otherwise it would not download anything and display the wrong contents.


## Priorities
(As in, what I think I need help on and what you might want to focus on, not what will only be accepted. All PRs will be considered, regardless if it's important or not)

1. Speed: if it's slower than going to pixiv then half of its purpose is gone
    * The bottleneck is network IO and downloading images from pixiv
2. Reliable rendering: There's no point in browsing a media-heavy site if the terminal is text-only and can't render the images well
    * While it's working perfectly for my use case, it should work well for other reasonable cases (different terminal sizes, number+name for the gallery)

Flowchart of modes and their connections:

![Flowchart UML](http://plantuml.com:80/plantuml/png/dPDD2y8m38Rl_HM5dZtejfk8YYY2Dy6BY1IDTHWtwGVYltVMhfkrAdWgIzuyUPUcGwMvrEQCX1W5Eww0ZgJEbTuAZWZorlNn-PaBwFdFQObONlD2RBajK8bFBO7BtR6Efmq1qLJaGrsPDKsjZIvb4u3BydGRem4I6A7zphgTtyXS77Ldu6f_oYkb-uNNhZtA5lnQp2H04ONuR0lnFCAq0mOD4ig4XR-Fp094pGud7pCZ0YDVcURYB2M1fPGo2NiIN9IjhE8nBv-alaKQjUjeqS5db3qkPfMN29gyBOUjRmJjuV-I8XpyOcHHN_znwuqBXqE6KEohHtG7)

Simplified UML diagram of the classes:

![Classes UML](http://plantuml.com:80/plantuml/png/bLXVZzmc47_dK_2OR6D_WEYqIjIeRQfMYjG_BrLbSJQxIuur572kf_I-UmSm633svbwYOpo_6MRcDpDitcb3b9ck72733AVuIjZOoYU4oBqsYlG6zvneZT_Fnr-4aFZkxjNxDrZHuVNn-1LX_DtNeFtixBrwZPuHce7AC6r-5GDrKAiE07m5WVzpGOZxpnGSCiudRRgJu5myTbQnum15AB_3VyqW7iUcSF0Mi_525JEwglxNXGq37Vzkjhma-0qDt2XslfXewOZ_PFJnarZkyEt8_EweD8GRJaylqYyT_4Q9wYz_QAhq2r0_k8kpyPm3IqvCh0qv_f5mJjNJYXbGtv7-Qpf9pXHgx0HIqIzmXKmc91qxpiQ7hXT23Nej7wTh4Df2Sa66Zbt1H84eFYOmvzH9WqHHgnbE2ualP1muxQGILvcHsKRiIf34-_qRt3-HSGbuZOxWZuuOFhrx9iwY8yvFT_PQHP3hxqcSQ_yvRGvpuvJ1lCKz88IcsdVd5eqJV2MUbPKQIsvOY2ru0rnSFuFLKhqYUcaZr7dL8djZIvB-0KW5ncQrUOvvaeELBgBcBmFJafCdJyQC7rPGPGJEUHrgZLs3v2BNSBd8jPoOT27cb7oXdj_rsLhc8SdnAaLjUon3pezE8mI9iXpiVRfhsygaGQ5KzLcrMp0Fh8p6NLGzGtNtDxPGKtEUyZSI8ZUVCsFHIgmLAjiJEsgNytgPBJOBWIlrAMuf9JS7wfAxjT-bYVCGaopiWPA84qIz68TjN_cegXJbiE6oc75bdhdkYOct_nt-UQ2gbcmiYRduFoSv44CJWdeZ3lnXRduT6GhR02uo7yvqf8eOeNQnzkgtyMZZxiscpQG0By9kp7cNuKDhyzxk-8mLi4EciYSExIGttEPGj9chPfB9hgYfxM1pS6s0XRtX5tM-EoQmCMzm9rxtWfkOYLQA3e-2ke6uI7sEZwLwQia_ed8xyvBoQ81dEiznC0ERIJlx-bcBCL8FH6vewAud_MMUixYVbAAZdp7sDLE8_Lia4DO11sybtNgJukMwN1eCdvZITZEUYuRpyyokgSzmpo2OXw3BCOippGVhpizfe2ufMPZBNKwCqv5SGwIpuVbrnOOu2RB0uQzxy2nEMPHE2Io7CoSA10DJHTXqVYVezDRX7Y4MUxRfoMOyfCSw2BAGVo_AGhP4p28k8c13Nc7hEfVKYkIq4gOamTfZ-XbU2Sn8mKRpBGKk6GA4jGde9EjJ9pvLYs150s3uYje7wFQcqcF9hB4OL50bmfoegXoJl79HRYA72Sr792X--lYJ-ocmJ7qGT_zCTAB9sLmK9SCYo7Utpy4ZOGRU_cSMkqCIfQ7OaDAYN-8mkQMWADn4c-Kz8GnC74jStnW3DOH57fx2vlQLAC3QxURgf_n7pARJG4LVu29zrTyDQHaGUaV7mVutoly0)

<details>
  <summary>Actor-ish model of the ui.Gallery class</summary>
  
  ![Gallery UML](http://plantuml.com:80/plantuml/png/bLLDRzim3BthLt2t16qFQT6BWRGTMc3OWBL1Y-vgQ1QJQB4bJPBJ1il--_HbnuxT0CibClb4FZsHr9snLDkwAedbjaJuGiiIzGugoT30nF_13AXWS3qbX1PMQdWrg0cPp-6xzneiiR1S2fZFYNV1nGtK7CxEiWYOWTjpJPpfmday8eFFPWDBPwuzdhCsaHJbRfgYX30PBFWxtpeCOTJIdzGWhFoA51gfRJvyy9kupa3H5UQYhiwXebpaYjfr0b0LUNrxUNe4ZRzhD9PejBKMZSqeAGps0xwuKpirMcTANPg2seGCQtSkni1gMcoSGj71v9ie9MKPFAGLyCeHD5vCllLn6WXf5ct0GFQhXyEbWmaSGXh_BFhZjbRd1svKINSn9SnEkoy_ZdwLikEGzCcGLzqj1xrQz49pOO6BfIGhBMi5IkCIRR7cOaN0_Jht4CGG-6xpMfV1cMwkP-jVjABQk5fmWCcqkwL_sKU9CdXLp1DslP3xOZ9vGOjr5pTognuO44E9l7LCEXIigRw5bfMoPB_VXyvwKYYIXCEkEZZ9iJp1clCsxF6ttueC_fHSRFv-QjFrItCaPHymIAkaiVal8qMJvs0urgjPvP7Q5ZLVScJkUaRwOKIpcA1b_kYkDaW_GZhu5WtVumergG_FNSsv8ZIn2YDTFtUwbNYKTA5nosLjFagiaZyKZpjP4kLauYRsCmQTXq5rSBXeaMAQenfX9simjSRRpeuCxgLOSbrHFvdxq-o-HAe7jPquFcDu44nu05fIFpY-fTgOh2HOcJuyQ3h2zO3_dr9f1ppl3dZU6V1yV1vt5M5Hv5XUasaIEMhSQmeLEixf6Gv9xbmp-cUGa7y0)
  
</details>

<details>
  <summary>Actor-ish model of the ui.Image class</summary>
  
  ![Image UML](http://plantuml.com:80/plantuml/png/bLHDJyCm3BtdLvWRQ73XMAbeIBk0n3PfshaAZJiYkYJ4ITd4-Eya_Tf71nezLCvxp-wp7NLCZbldroLpqfK8Jsk-GdZH0XbBqpe0mX9p9xM2D6KyTzh2aj2o-8Ax1_0IHgEaqTwpS0fOv19uf7SeWjn7fHG76GdCvKPM4MmIk6cgF2zcKx3uuP4Si-YyLHr6HYj29hZZhvmmv8QeJQ_Z11R17D9Usv12VwfISv70f8r0nZufTYChxh2NC853hBKnaMHAlgKc-HQCbSg5aoeqs-rszS1c1bN3ns6TJ6XFTYKZWWA-IgdUlw_wAiSsprGw5WpQxAAifhCAhImaYkkRVpNSsvdYnlrgHGKoCu4BrKzzSDggFElTa95AeRtOHYpNtwM_fh-osXkOMopGvM-rfNOo49w36x9tBDUhpDko5hJB6E0NjnF5G_iHFTVMYQS4baOS2h3T6p5KWrs49YkfFO4vlmxJyjrABhsxu_2j-1jWFm00)

</details>

<details>
  <summary>Actor-ish model of the ui.User class</summary>
  
  ![User UML](http://plantuml.com:80/plantuml/png/VLHDJzmm4BtxLymHX3Z0zXHfAQX8g9LALqKzHsuokrWaTZgs5rRQ7r-FFx9Pmf93vCnxRpuplhLEalDz9vno7S8VYsJXh3SjmW8CmPTp8PPB-Ca6YnqsJXaDowMGZGimymM_uF86l8ABYofnarW4qsL0c571dNs1TJ1xvgiswwAmWfLwKrksikG6v92S_NZwzGY4_pnQ9mYT9rriTZ3QecYbat-bHV1y4WsXTEKZ-Ohd2fv2mcWFmipiR2DTPiPpIBHjmP5iNa9n2MZDg-wCR2kupoTGlToWvJweBVif554EeyDOo7UcmtMWQPnJ2dwLy2GR6tUlxD39NGe9Lv_3rwUzBt9qd2VzgSl5L9BwjI7Z1nW8r-YQPwKt0i8pwNSiMkERgprr4SpJEx8T3tkGPB5cmjdvL78yy7U1veCz44wFZJdpBh3re-wWRilFjoBJ3qxiz_ku68yXsP1tQ5BOYNUB4B5Lm8xNm3wRrvebeSXSH_Z_Iojpd370Yd2hZBUdWWmPBnvpcjCWnTM3WR3ioZg8-pttjJ5r8jHCEAGtpZEZlC2r6bloiVKkMXzaro7jnXn-Ovp2F3P5O8oPRmn2s1aFrXD-dIWg-6RqMb4l-JeQQ-QxhDGhRiFG-68JuZy0)
  
</details>

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

## Manual installation

Note: if you want to make some edits, you should install it in a conda environment. See above

```sh
# Use the latest stable version (recommended for usage)
# Make sure the version number is the latest
git clone -b 'v0.6' --depth 1 https://github.com/twenty5151/koneko.git
# Use the master branch for upcoming features:
git clone -b master https://github.com/twenty5151/koneko.git
# Use the dev branch for latest features, fixes, and instability (recommended for contributers):
git clone -b dev https://github.com/twenty5151/koneko.git

# Run the tests (for those who want to edit)
pytest testing/ -vvvv -l -s --icat

cd koneko
# Manually install without PyPI; for general usage
# Both will correctly copy the required pictures
pip install .
# or
python setup.py install
# or
# Manually install for development, changes will be immediately reflected
python setup.py develop

# On certain shells with implicit cd, typing `koneko` might cd into the dir
# Instead of running the executable
cd ~
# Use anywhere:
koneko
```

## Unit tests
Run `pytest testing/ -vvvv -l -s --icat`. If icat fails, skip the three offending tests by omitting the `--icat` argument

## Upload to PyPI
When test installing with pip, don't forget to use `pip install .` or `python setup.py install`, not `pip install koneko` (which will grab from latest stable version). (Yes, I made the same mistake again)

Bump version info in `__init__.py`, `setup.py`, and `README.md`

```sh
python setup.py sdist bdist_wheel
twine upload dist/*
pip install koneko --upgrade
```

# Manual

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
```

```
Artist Gallery commands: (No need to press enter)
    Using coordinates, where {digit1} is the row and {digit2} is the column
    {digit1}{digit2}   -- display the image on row digit1 and column digit2
    o{digit1}{digit2}  -- open pixiv image/post in browser
    d{digit1}{digit2}  -- download image in large resolution

Using image number, where {number} is the nth image in order (see examples)
    i{number}          -- display the image
    O{number}          -- open pixiv image/post in browser.
    D{number}          -- download image in large resolution.

    n                  -- view the next page
    p                  -- view the previous page
    r                  -- delete all cached images, re-download and reload view
    b                  -- go back to previous mode (either 3, 4, 5, or main screen)
    h                  -- show this help
    q                  -- quit (with confirmation)

Examples:
    i09   --->  Display the ninth image in image view (must have leading 0)
    i10   --->  Display the tenth image in image view
    O9    --->  Open the ninth image's post in browser
    D9    --->  Download the ninth image, in large resolution

    25    --->  Display the image on column 2, row 5 (index starts at 1)
    d25   --->  Open the image on column 2, row 5 (index starts at 1) in browser
    o25   --->  Download the image on column 2, row 5 (index starts at 1)
```

```
Image view commands (No need to press enter):
    b -- go back to the gallery
    n -- view next image in post (only for posts with multiple pages)
    p -- view previous image in post (same as above)
    d -- download this image
    o -- open pixiv post in browser
    f -- show this image in full resolution

    h -- show keybindings
    m -- show this manual
    q -- quit (with confirmation)
```

```
User view commands (No need to press enter):
    {digit1}{digit2}   -- display artist illusts on column digit1 and row digit2
    n                  -- view next page
    p                  -- view previous page
    r                  -- delete all cached images, re-download and reload view
    h                  -- show keybindings
    m                  -- show this manual
    q                  -- quit (with confirmation)
```

```
Illust Follow Gallery commands: (No need to press enter)
    Using coordinates, where {digit1} is the row and {digit2} is the column
    {digit1}{digit2}   -- display the image on row digit1 and column digit2
    o{digit1}{digit2}  -- open pixiv image/post in browser
    d{digit1}{digit2}  -- download image in large resolution
    a{digit1}{digit2}  -- view illusts by the artist of the selected image

Using image number, where {number} is the nth image in order (see examples)
    i{number}          -- display the image
    O{number}          -- open pixiv image/post in browser.
    D{number}          -- download image in large resolution.
    A{number}          -- view illusts by the artist of the selected image

    n                  -- view the next page
    p                  -- view the previous page
    r                  -- delete all cached images, re-download and reload view
    b                  -- go back to main screen
    h                  -- show this help
    q                  -- quit (with confirmation)

Examples:
    i09   --->  Display the ninth image in image view (must have leading 0)
    i10   --->  Display the tenth image in image view
    O9    --->  Open the ninth image's post in browser
    D9    --->  Download the ninth image, in large resolution

    25    --->  Display the image on column 2, row 5 (index starts at 1)
    d25   --->  Open the image on column 2, row 5 (index starts at 1) in browser
    o25   --->  Download the image on column 2, row 5 (index starts at 1)
```

## Trackers
Nine trackers in the Android app, according to [exodus](https://reports.exodus-privacy.eu.org/en/reports/jp.pxv.android/latest/):

* Amazon Advertisement
* AMoAd
* Google Ads
* Google CrashLytics
* Google DoubleClick
* Google Firebase Analytics
* Integral Ad Science
* Moat
* Twitter MoPub

Advertisers from pixiv's [privacy policy](https://policies.pixiv.net/en.html#booth):

* Looker
* Repro
* Qualaroo
* DDAI（Date Driven Advertising Initiative）
* YourAdChoices
* Rubicon Project
* i-Mobile Co., Ltd.
* Akinasista Corporation
* Axel Mark Inc.
* AppLovin
* Amazon Japan G.K.
* AmoAd Inc.
* AOL Platforms Japan K.K.
* OpenX
* Google Inc.
* CRITEO K.K.
* CyberAgent, Inc.
* Geniee, Inc.
* Supership Inc.
* GMO AD Marketing Inc.
* F@N Communications, Inc.
* Facebook Inc.
* Fluct, Inc.
* Platform One Inc.
* MicroAd Inc.
* MoPub Inc.
* Yahoo! Japan Corporation
* United, Inc.
* 株式会社Zucks
* PubMatic, Inc.
* Liftoff Mobile, Inc.
* Mobfox US LLC
* OneSignal
* Smaato, Inc.
* SMN株式会社
* 株式会社アドインテ
