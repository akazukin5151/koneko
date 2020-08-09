# koneko

[![GPLv3 license](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.txt) [![PyPI](https://img.shields.io/pypi/v/koneko)](https://pypi.org/project/koneko/) [![commits since](https://img.shields.io/github/commits-since/twenty5151/koneko/latest)](https://GitHub.com/twenty5151/koneko/commit/) ![master](https://github.com/twenty5151/koneko/workflows/master/badge.svg?branch=master) ![dev](https://github.com/twenty5151/koneko/workflows/dev/badge.svg?branch=dev)

> Browse pixiv in the terminal using kitty's icat to display images (or use ueberzug)

Gallery view
![Gallery view_square_medium1](pics/gallery_view_square_medium1.png)
![Gallery view_square_medium2](pics/gallery_view_square_medium2.png)
Image view
![Image_view](pics/image_view.png)
Artist search (artist profile picture on the left, 3 previews on right)
![artist_search](pics/artist_search.png)
View artists you're following
![following_users_view](pics/following_users_view.png)

Requires [kitty](https://github.com/kovidgoyal/kitty). It uses the magical `kitty +kitten icat` 'kitten' to display images. For more info see the [kitty documentation](https://sw.kovidgoyal.net/kitty/kittens/icat.html). Actually, `lscat.py` uses [pixcat](https://github.com/mirukana/pixcat), which is a Python API for icat.

*New in version 0.11.0: [ueberzug](https://github.com/seebye/ueberzug) is now supported as an experimental feature, enable through the config file.*

**Why the name Koneko?** Koneko (こねこ) means kitten, which is what `icat` is, a kitty `+kitten`


# Features (what?)
See the [manual](MANUAL.md) for more details

1. View artist illustrations ([ex](https://www.pixiv.net/bookmark.php?type=user))
2. View a post ([ex](https://www.pixiv.net/en/artworks/78823485))
    - View related images suggested by pixiv (ex: scroll down from the above example)
3. View the artists that you are following (or any other user ID) ([ex](https://www.pixiv.net/bookmark.php?type=user))
4. Search for an artist/user ([ex](https://www.pixiv.net/search_user.php?nick=raika9&s_mode=s_usr))
5. View new illustrations from all the artists you are following ([ex](https://www.pixiv.net/bookmark_new_illust.php))
6. View recommended illustrations (now called 'discovery') ([ex](https://www.pixiv.net/discovery))


* Navigate between next and previous pages/images
* Download images ([PixivUtil](https://github.com/Nandaka/PixivUtil2/) would be more suitable for batch download) in full resolution
* Open post in browser
* Browse an offline cache


# Why?
* Terminal user interfaces are minimalist, fast, and doesn't load Javascript that slows down your entire browser or track you
    * Image loading is *so* much faster, especially if you don't delete the cache

I get 32 trackers on Pixiv. Plus, you have to disable ublock if you ever get logged out

<a href="url"><img src="pics/pixiv_ublock.png" height="350"></a>

The mobile app even directly tells you Google "and our 198 partners" "collect and use data"! See [prompt 1](https://raw.githubusercontent.com/twenty5151/koneko/master/pics/ads1.png), [prompt 2](https://raw.githubusercontent.com/twenty5151/koneko/master/pics/ads2.png) (Github can't render the images correctly for some reason) and this [list](#trackers-avoided)

* TUIs make you cool
* TUIs *with embedded pictures* make you even cooler
* TUIs embedded with pictures of cute anime girls make you the coolest
* Keyboard driven
* Familiar, vim-like key sequences
* I use arch btw


# Installation (how?)
See also: [manual installation](CONTRIBUTING.md#manual-installation)

0. If you want to use the stable api, install [kitty](https://github.com/kovidgoyal/kitty) (Otherwise, you can use ueberzug with your current terminal, however note that it doesn't seem to work on macOS)
    * If using ueberzug, run `pip install ueberzug` first
1. `pip install koneko` (or if you use [conda](CONTRIBUTING.md#conda-environment)...):
2. Run `koneko` to login and save credentials
3. Run `lscat 1 7` to help setup the recommended settings; copy to `~/.config/koneko/config.ini`. (Don't skip this step! Image display in the terminal is very sensitive to your config!)
4. See [usage](#usage) for how to use.

## Requirements

* Python 3.8+
* It has been tested on kitty v0.17.2 onwards, but should work on older versions
* Operating system: all OSes that kitty supports, which means Linux and macOS.
    * Ueberzug only works on linux
* Uses `xdg-open` (linux) / `open` (mac) (for opening links in your browser) and `curl` (for safety fallback, see below)

<details>
  <summary>If it crashes (it shouldn't), it might be because pip didn't 'install' the welcome pictures, *and* the script failed to download them for some reason. Try:</summary>

```sh
mkdir -p ~/.local/share/koneko/pics
   
curl -s https://raw.githubusercontent.com/twenty5151/koneko/master/pics/71471144_p0.png -o ~/.local/share/koneko/pics/71471144_p0.png
    
curl -s https://raw.githubusercontent.com/twenty5151/koneko/master/pics/79494300_p0.png -o ~/.local/share/koneko/pics/79494300_p0.png
 ```
</details>

# Usage and manual

See the [manual](MANUAL.md) here


## Upcoming changelog (in dev branch)

For full changelogs please see [releases](https://github.com/twenty5151/koneko/releases)

### Version 0.11.0

#### Features
* Ueberzug support to work with other terminals (but seems to be linux only)
    * New configuration options in the experimental section
        * `use_ueberzug` (default: off)
        * `scroll_display` (default: on)
            * On means you have to manually scroll the terminal with mouse/keyboard shortcuts
            * Off means you can use up/down keys to view more images
            * This option is always on if `use_ueberzug` is on
        * `ueberzug_center_spaces` (default: 20)
    * If `scroll_display` is off, then use up/down keys to view more images
    * Should work on everything:
         * lscat app
             * In lscat config assistant
                 * Page spacing is disabled for ueberzug
             * Browse cache, display path
             * testgallery, testuser
         * koneko app
             * For already downloaded images in Gallery, Users, & Image modes
             * For showing images as they download in Gallery, Users, & Image modes
             * Everywhere else (main screen, info screen, etc)
* Improved documentation using tables in the manual (MANUAL.md) and developer guide (HACKING.md)

#### Bug fixes
* Fixed bugs in lscat app's display path (mode p/3):
    * Not working if path given via cli
    * Always showing directory in gallery mode
* Fixed race conditions while prefetching
    * eg, viewing an image while prefetching would crash or cause undefined behaviour
    * Prefetching in user mode takes longer, making it likely to trigger an action before prefetch has finished

#### Code maintenance
* Remove circular imports
    - [x] prompt importing ui
    - [x] assistants importing lscat_app
    - [x] main importing cli
    - [x] picker importing assistants
* Updated and improved diagrams in HACKING.md
* Methods starting with `maybe` (formerly abstractmethods) now do nothing by default, override to do something
    * Rename middle() to maybe_show_preview() in lscat prompt
* Inline in yield expression in users preview (lscat)
* Replace while loop with a for loop and a guard at the end in lscat
* Use Keystroke name attribute rather than its key code (blessed.Terminal)
* Rename utils.check_quit() to quit_on_q()
* Move lscat related functions in utils to lscat
* print_doc now restores previous cursor location
* Rewrite of core lscat functions: **new unified API (lscat.api)** with the same interface for both pixcat and ueberzug
    * For ueberzug, only one ueberzug process and canvas is instantiated in the program
* Added more tests


# Roadmap

## Features

* Refactor config functions and confusing extraction of Results. See rust implementation of the config in koneko-rs
    * Keep in mind that 'on'/'off' is allowed, which complicates parsing into types
* Consider using reStructuredText and automate building and hosting of docs
* Some info (eg manual) are still being hidden by ueberzug; general unreliability of prints (need a rethink of implementation)
    * Help message that responds to terminal width
* Refine ueberzug dependency in setup.py/requirements.txt, keeping in mind that ueberzug only builds on linux
* Illust-related mode can't go back to image mode
* Support illust related mode in lscat app mode 2/b
* In-depth usage documentation; use letters to represent modes (at least in public docs) rather than numbers
    * Make sure the diagrams in HACKING.md is up-to-date
* Option to save username, but prompt for password (and not save it) every time

## Known bugs

* In the logs, urllib3 warns that `Connection pool is full, discarding connection: i.pximg.net`. See [customising pool behaviour](https://urllib3.readthedocs.io/en/latest/advanced-usage.html#customizing-pool-behavior) from urllib3.
* Prefetch thread still running (downloading) hangs the entire app, even when user quits. Cannot use daemon threads as it still hangs then noisly aborts. Changing prompt.ask_quit() into a UI method so that it can pass a threading.Event() to downloads, doesn't work either as all the downloads has already been submitted to the ThreadPoolExecutor before the user is quick enough to send 'q'. The only way is to interrupt the urllib download process, which is going to be unsafe if you don't know what you're doing.
* There seems to be a delay between entering `koneko` and startup, but the delay is before the first line of the script even executes. Import time is fast. `pip install` using the wheel seems to reduce the delay. Directly running the script using `python koneko/main.py` or `python koneko/lscat_app.py` is faster as well. Seems like it's a delay in going to `miniconda/lib/python3.8/site-packages/koneko.egg-link` (installed with `python setup.py develop`)?

# FAQ
* I'm having problems with lscat

For the best experience use the terminal in full screen, unless your screen is big enough. Moving and resizing it abruptly will not be good for icat, which is really kitty's problem not mine. Extra information can be disabled from being printed.

You can also use versions less than v0.5.1, which retains legacy support for the original lsix shell script. Note that I've never really tested it, which is why I decided to be honest and depreciated legacy support from v0.6 onwards. The current lscat API has matured to the point where it's simple to write a replacement anyway.

# Contributing
1. Fork it
2. Run tests with `pytest testing/ -vvvv -l`
3. Make your changes
4. Run tests again (add `-s --inte` for integration tests if you want)
5. Submit a pull request

Tips: 
* See [HACKING.md](HACKING.md) to understand the code.
* If your git client complains about committing to master, just remove `.pre-commit-config.yaml`
* If you want to, you can create an issue first. Ask any questions by opening a new issue.
* If you're encountering/fixing a bug and you're stuck, try clearing the cache. For example, a bug might have downloaded to the wrong folder, but after fixing the bug, you need to clear the cache, otherwise it would not download anything and display the wrong contents.

See the rest in [CONTRIBUTING.md](CONTRIBUTING.md)

## Unit tests
Run `pytest testing/ -vvvv -l`. Add `-s --inte` for integration testing, but don't be surprised if it fails, because integration tests require a valid config/account + internet connection

## Build and upload to PyPI

0. Run integration tests locally
1. Review github action logs to make sure nothing is wrong
2. Bump version info in `__init__.py`, `setup.py`, and `CONTRIBUTING.md`
3. Run:

```sh
# Change 1st argument to where [`plantuml.jar`](https://plantuml.com/download) is stored
java -jar ~/Applications/plantuml.jar puml/classes -o render
python setup.py sdist bdist_wheel
twine upload dist/*
pip install koneko --upgrade
```

# Trackers avoided
<details>
<summary>This is a list of trackers present when you use the official pixiv website or app. koneko frees you from them.</summary>

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
</details>
