koneko
======


.. image:: https://img.shields.io/badge/License-GPLv3-blue.svg
   :target: https://www.gnu.org/licenses/gpl-3.0.txt
   :alt: GPLv3 license

.. image:: https://img.shields.io/pypi/v/koneko
   :target: https://pypi.org/project/koneko/
   :alt: PyPI

.. image:: https://img.shields.io/github/commits-since/akazukin5151/koneko/latest
   :target: https://GitHub.com/akazukin5151/koneko/commit/
   :alt: commits since

.. image:: https://github.com/akazukin5151/koneko/workflows/master/badge.svg?branch=master
   :target: https://github.com/akazukin5151/koneko/workflows/master/badge.svg?branch=master
   :alt: master

.. image:: https://readthedocs.org/projects/koneko/badge/?version=latest
    :target: https://koneko.readthedocs.io/en/latest/?badge=latest
    :alt: docs


..

   Browse pixiv in the terminal using kitty's icat to display images (or use ueberzug)


Gallery view

.. image:: /pics/gallery_view_square_medium1.png
   :target: /pics/gallery_view_square_medium1.png
   :alt: Gallery view_square_medium1


.. image:: /pics/gallery_view_square_medium2.png
   :target: /pics/gallery_view_square_medium2.png
   :alt: Gallery view_square_medium2

Image view

.. image:: /pics/image_view.png
   :target: /pics/image_view.png
   :alt: Image_view

Artist search (artist profile picture on the left, 3 previews on right)

.. image:: /pics/artist_search.png
   :target: /pics/artist_search.png
   :alt: artist_search

View artists you're following

.. image:: /pics/following_users_view.png
   :target: /pics/following_users_view.png
   :alt: following_users_view


For the `icat <https://sw.kovidgoyal.net/kitty/kittens/icat.html>`_, backend, the `kitty <https://github.com/kovidgoyal/kitty>`_ terminal is needed. Alternatively, the `ueberzug <https://github.com/seebye/ueberzug>`_ backend can be used in any terminal in X11 linux.

**Why the name Koneko?** Koneko (こねこ) means kitten, which is what ``icat`` is, a kitty ``+kitten``

Features
--------

See the :ref:`manual <manual>` for more details


#. View artist illustrations (\ `ex <https://www.pixiv.net/bookmark.php?type=user>`_\ )
#. View a post (\ `ex <https://www.pixiv.net/en/artworks/78823485>`_\ )

   * View related images suggested by pixiv (ex: scroll down from the above example)

#. View the artists that you are following (or any other user ID) (\ `ex <https://www.pixiv.net/bookmark.php?type=user>`_\ )
#. Search for an artist/user (\ `ex <https://www.pixiv.net/search_user.php?nick=raika9&s_mode=s_usr>`_\ )
#. View new illustrations from all the artists you are following (\ `ex <https://www.pixiv.net/bookmark_new_illust.php>`_\ )
#. View recommended illustrations (now called 'discovery') (\ `ex <https://www.pixiv.net/discovery>`_\ )


* Save images individually in full resolution (\ `PixivUtil <https://github.com/Nandaka/PixivUtil2/>`_ would be more suitable for batch download)
* Open post in browser

* Browse the offline cache
* Frequently used modes and searches

Why?
----


* Terminal user interfaces are minimalist, fast, and doesn't load Javascript that slows down your entire browser or track you

  * Image loading is *so* much faster, especially if you don't delete the cache

I get 32 trackers on Pixiv. Plus, you have to disable ublock if you ever get logged out

.. image:: /pics/pixiv_ublock.png
   :target: /pics/pixiv_ublock.png
   :alt: pixiv_ublock
   :scale: 50%
   :align: center

The mobile app even directly tells you Google "and our 198 partners" "collect and use data"! See `prompt 1 <https://raw.githubusercontent.com/akazukin5151/koneko/master/docs/pics/ads1.png>`_ , `prompt 2 <https://raw.githubusercontent.com/akazukin5151/koneko/master/docs/pics/ads2.png>`_ and this `list <#trackers-avoided>`_ of trackers


* TUIs make you cool
* TUIs *with embedded pictures* make you even cooler
* TUIs embedded with pictures of cute anime girls make you the coolest
* Keyboard driven
* Familiar, vim-like key sequences
* I use arch btw

Installation
------------


Requirements
^^^^^^^^^^^^

* Python 3.8+
* It has been tested on kitty v0.17.2 onwards, but should work on older versions
* Operating system: all OSes that kitty supports, which means Linux and macOS.

  * Ueberzug only works on Linux systems with X11

* Dependencies on external programs (your responsibility to install them):

  * ``xdg-open`` (linux) or ``open`` (mac) for opening links in your browser
  * ``curl`` to download koneko's welcome and about images if they are missing (not needed unless if you deleted them)
  * ``xdg-mime`` and ``update-desktop-database`` to handle the pixiv login callback

    * For ``update-desktop-database``, try install the ``desktop-file-utils`` package with your package manager


.. raw:: html

   <details>
     <summary>If koneko crashes (it shouldn't), it might be because pip didn't 'install' the welcome pictures, *and* the program failed to download them for some reason. Try:</summary>

   <pre><code>
   mkdir -p ~/.local/share/koneko/pics

   curl -s https://raw.githubusercontent.com/akazukin5151/koneko/master/pics/71471144_p0.png -o ~/.local/share/koneko/pics/71471144_p0.png

   curl -s https://raw.githubusercontent.com/akazukin5151/koneko/master/pics/79494300_p0.png -o ~/.local/share/koneko/pics/79494300_p0.png
    </code></pre>
   </details>


Steps
^^^^^

See also: :ref:`manual installation <manual-installation>`


#. If not using `kitty <https://github.com/kovidgoyal/kitty>`_, you have to use the ueberzug backend. Follow the instructions `here <https://github.com/WhiteBlackGoose/ueberzug-latest>`_
#. ``pip install koneko`` (or if you use :ref:`conda <conda-environment>`...)
#. Run ``koneko``. It will open a pixiv login page in your default browser and quit.
#. Login to pixiv on your browser.
#. If prompted, open the ``pixiv://`` link with "koneko pixiv login handler".
#. If successful you should see a notification saying "Login finished!". If not, make a bug report at https://github.com/akazukin5151/koneko/issues/
#. Run ``lscat 1 8`` to get the recommended settings for your screen size

    * (Don't skip this step! Image display in the terminal is very sensitive to your config!)

    * If you get command not found, try adding ``$HOME/.local/bin`` to your ``$PATH``

#. Copy the recommended settings to ``~/.config/koneko/config.ini``. See :ref:`example_config.ini <example_config.ini>` for reference

#. Run ``koneko`` again. Hopefully you don't see any error messages about login failing.

#. See the :ref:`usage manual <manual>` for how to use.


Usage and manual
----------------

See :ref:`MANUAL.rst <manual>`

FAQ
---


I'm having problems logging in
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Try these steps in order:

- Update your system and reboot. No seriously, that's what worked for me.
- Try a different browser
- Set said different browser as your default browser
- Make a bug report at https://github.com/akazukin5151/koneko/issues/ for support and debugging
- Use the original script `here <https://gist.github.com/ZipFile/c9ebedb224406f4f11845ab700124362>`_ to get your refresh token. Copy the example config to ``~/.config/koneko``, and add the line ``refresh_token=XXXX`` under the ``[Credentials]`` section.

I'm having problems with lscat
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

For the best experience use the terminal in full screen, unless your screen is big enough. Moving and resizing it abruptly will not be good for icat, which is really kitty's problem not mine. icat moves the cursor around, which makes it prone to creating buggy behavior when text is printed. You can disable such text with ``print_info = off`` under ``[misc]``


Contributing
------------

See :ref:`CONTRIBUTING.rst <contributing>`


Trackers avoided
----------------


.. raw:: html

   <details>
   <summary>This is a list of trackers present when you use the official pixiv website or app. koneko frees you from them.</summary>

   Nine trackers in the Android app, according to <a href=https://reports.exodus-privacy.eu.org/en/reports/jp.pxv.android/latest/>exodus</a>:

   <ul>
       <li>Amazon Advertisement</li>
       <li>AMoAd</li>
       <li>Google Ads</li>
       <li>Google CrashLytics</li>
       <li>Google DoubleClick</li>
       <li>Google Firebase Analytics</li>
       <li>Integral Ad Science</li>
       <li>Moat</li>
       <li>Twitter MoPub</li>
   </ul>

   Advertisers from pixiv's <a href=https://policies.pixiv.net/en.html#booth>privacy policy</a>:

   <ul>
       <li>Looker</li>
       <li>Repro</li>
       <li>Qualaroo</li>
       <li>DDAI（Date Driven Advertising Initiative）</li>
       <li>YourAdChoices</li>
       <li>Rubicon Project</li>
       <li>i-Mobile Co., Ltd.</li>
       <li>Akinasista Corporation</li>
       <li>Axel Mark Inc.</li>
       <li>AppLovin</li>
       <li>Amazon Japan G.K.</li>
       <li>AmoAd Inc.</li>
       <li>AOL Platforms Japan K.K.</li>
       <li>OpenX</li>
       <li>Google Inc.</li>
       <li>CRITEO K.K.</li>
       <li>CyberAgent, Inc.</li>
       <li>Geniee, Inc.</li>
       <li>Supership Inc.</li>
       <li>GMO AD Marketing Inc.</li>
       <li>F@N Communications, Inc.</li>
       <li>Facebook Inc.</li>
       <li>Fluct, Inc.</li>
       <li>Platform One Inc.</li>
       <li>MicroAd Inc.</li>
       <li>MoPub Inc.</li>
       <li>Yahoo! Japan Corporation</li>
       <li>United, Inc.</li>
       <li>株式会社Zucks</li>
       <li>PubMatic, Inc.</li>
       <li>Liftoff Mobile, Inc.</li>
       <li>Mobfox US LLC</li>
       <li>OneSignal</li>
       <li>Smaato, Inc.</li>
       <li>SMN株式会社</li>
       <li>株式会社アドインテ</li>
   </ul>

   </details>

