Note: the [haskell](https://github.com/akazukin5151/koneko/tree/haskell) branch is an ongoing rewrite of koneko in Haskell using Brick, to enable a more responsive, less buggy, and easier to maintain code

# koneko

[![GPLv3 license](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.txt) [![PyPI](https://img.shields.io/pypi/v/koneko)](https://pypi.org/project/koneko/) [![commits since](https://img.shields.io/github/commits-since/akazukin5151/koneko/latest)](https://GitHub.com/akazukin5151/koneko/commit/) ![master](https://github.com/akazukin5151/koneko/workflows/master/badge.svg?branch=master) [![docs](https://readthedocs.org/projects/koneko/badge/?version=latest)](https://koneko.readthedocs.io/en/latest/?badge=latest) [![status](https://img.shields.io/badge/status-maintenance-green)](https://img.shields.io/badge/status-maintenance-green)

> Browse pixiv in the terminal using kitty's icat to display images (or ueberzug for all terminals)

Gallery view 1
![Gallery view_square_medium1](docs/pics/gallery_view_square_medium1.png)
Gallery view 2
![Gallery view_square_medium2](docs/pics/gallery_view_square_medium2.png)
Image view
![Image_view](docs/pics/image_view.png)
Artist search (artist profile picture on the left, 3 previews on right)
![artist_search](docs/pics/artist_search.png)
View artists you're following
![following_users_view](docs/pics/following_users_view.png)

For the [icat](https://sw.kovidgoyal.net/kitty/kittens/icat.html), backend, the [kitty](https://github.com/kovidgoyal/kitty) terminal is needed. Alternatively, the [ueberzug](https://github.com/WhiteBlackGoose/ueberzug-latest) backend can be used in any terminal in X11 linux.

**Why the name Koneko?** Koneko (こねこ) means kitten, which is what `icat` is, a kitty `+kitten`


## Features
See the [manual](MANUAL.md) for more details

1. View artist illustrations ([eg](https://www.pixiv.net/bookmark.php?type=user))
2. View a post ([eg](https://www.pixiv.net/en/artworks/78823485))
    - View related images suggested by pixiv (eg: scroll down from the above example)
3. View the artists that you are following (or any other user ID) ([eg](https://www.pixiv.net/bookmark.php?type=user))
4. Search for an artist/user ([eg](https://www.pixiv.net/search_user.php?nick=raika9&s_mode=s_usr))
5. View new illustrations from all the artists you are following ([eg](https://www.pixiv.net/bookmark_new_illust.php))
6. View recommended illustrations (now called 'discovery') ([eg](https://www.pixiv.net/discovery))

* Save images individually in full resolution ([PixivUtil](https://github.com/Nandaka/PixivUtil2/) would be more suitable for batch download)
* Open post in browser

* Browse the offline cache
* Frequently used modes and searches


## Why?
* Terminal user interfaces are minimalist, fast, and doesn't load Javascript that slows down your entire browser or track you
    * Image loading is *so* much faster, especially if you don't delete the cache

I get 32 trackers on Pixiv. Plus, you have to disable ublock if you ever get logged out

<a href="url"><img src="docs/pics/pixiv_ublock.png" height="350"></a>

The mobile app even directly tells you Google "and our 198 partners" "collect and use data"! See [prompt 1](https://raw.githubusercontent.com/akazukin5151/koneko/master/docs/pics/ads1.png), [prompt 2](https://raw.githubusercontent.com/akazukin5151/koneko/master/docs/pics/ads2.png) and this [list](#trackers-avoided) of trackers

* TUIs make you cool
* TUIs *with embedded pictures* make you even cooler
* TUIs embedded with pictures of cute anime girls make you the coolest
* Keyboard driven
* Familiar, vim-like key sequences
* I use arch btw


## Installation

### Requirements

* Python 3.8+
* icat:
    * It has been tested on kitty v0.17.2 onwards, but should work on older versions
    * Linux and macOS
* Ueberzug: Linux with X11
* Dependencies on external programs (your responsibility to install them):
    - `xdg-mime` and `update-desktop-database` to handle the pixiv login callback
        - For `update-desktop-database`, try install the `desktop-file-utils` package with your package manager
    - `xdg-open` (linux) or `open` (mac) for opening links in your browser
    - `curl` to download koneko's welcome and about images if they are missing (not needed unless if you deleted them)

<details>
  <summary>If koneko crashes (it shouldn't), it might be because pip didn't 'install' the welcome pictures, *and* the program failed to download them for some reason. Try:</summary>

```sh
mkdir -p ~/.local/share/koneko/pics
   
curl -s https://raw.githubusercontent.com/akazukin5151/koneko/master/pics/71471144_p0.png -o ~/.local/share/koneko/pics/71471144_p0.png
    
curl -s https://raw.githubusercontent.com/akazukin5151/koneko/master/pics/79494300_p0.png -o ~/.local/share/koneko/pics/79494300_p0.png
 ```
</details>

### Steps

See also: [manual installation](docs/pages/CONTRIBUTING.rst#manual-installation)

0. If not using [kitty](https://github.com/kovidgoyal/kitty), you have to use the ueberzug backend. Follow the instructions to install ueberzug [here](https://github.com/WhiteBlackGoose/ueberzug-latest)
1. `pip install koneko` (or if you use [conda](docs/pages/CONTRIBUTING.rst#conda-environment)...)
2. Run `koneko`. It will open a pixiv login page in your default browser and quit.
3. Login to pixiv in your browser.
4. If prompted, open the `pixiv://` link with "koneko pixiv login handler".
5. If successful you should see a notification saying "Login finished!". If not, make a bug report at https://github.com/akazukin5151/koneko/issues/
6. Run `lscat 1 8` to get the recommended settings for your screen size.
    * (Don't skip this step! Image display in the terminal is very sensitive to your settings!)
    * If you get command not found, try adding `$HOME/.local/bin` to your `$PATH`
7. Copy the recommended settings to `~/.config/koneko/config.ini`. See [example_config.ini](example_config.ini) for reference
8. If using ueberzug, add `use_ueberzug = on` under `[experimental]` in the config
9. Run `koneko` again. Hopefully you don't see any error messages about login failing. 
10. See [usage](#usage) for how to use.

## Usage and manual

See the [MANUAL.md](MANUAL.md)


## FAQ
### I'm having problems logging in

Try these steps in order:

- If you successfully logged in, but the redirect did not work (the handler did not launch), ensure `koneko-url-login` is in your `$PATH`
    - eg, look at where it is with `whereis koneko-url-login` and symlink it to `$HOME/.local/bin/`
- Update your system and reboot. No seriously, that's what worked for me.
- Try a different browser
- Set said different browser as your default browser
- Make a bug report at https://github.com/akazukin5151/koneko/issues/ for support and debugging
- Use the original script [here](https://gist.github.com/ZipFile/c9ebedb224406f4f11845ab700124362) to get your refresh token. Copy the example config to `~/.config/koneko`, and add the line `refresh_token=XXXX` under the `[Credentials]` section.

### I'm having problems with lscat

For the best experience use the terminal in full screen, unless your screen is big enough. Moving and resizing it abruptly will not be good for icat, which is really kitty's problem not mine. icat moves the cursor around, which makes it prone to creating buggy behavior when text is printed. You can disable such text with `print_info = off` under `[misc]`

## Contributing

See [CONTRIBUTING.rst](docs/pages/CONTRIBUTING.rst)

## Trackers avoided
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
