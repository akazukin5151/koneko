.. _contributing:

Contributing
============


#. Fork it
#. Run tests with ``pytest testing/ -vvvv -l``
#. Make your changes
#. Run tests again
#. You can also double check the docs by running ``cd docs`` then ``make html``
#. Submit a pull request

Tips: 


* If your git client complains about committing to master, just remove ``.pre-commit-config.yaml``
* If you want to, you can create an issue first. Ask any questions by opening a new issue.
* If you're encountering/fixing a bug and you're stuck, try clearing the cache. For example, a bug might have downloaded to the wrong folder, but after fixing the bug, you need to clear the cache, otherwise it would not download anything and display the wrong contents.

Priorities
----------

(As in, what I think I need help on and what you might want to focus on, not what will only be accepted. All PRs will be considered, regardless if it's important or not)


#. Speed: if it's slower than going to pixiv then half of its purpose is gone

   * The bottleneck is network IO and downloading images from pixiv. Other than that, the next bottleneck is pixcat. It creates a new temporary image (converted to png) every time, but the core mechanic is just printing escape codes. I suspect it can be made faster.

#. Reliable rendering: There's no point in browsing a media-heavy site if the terminal is text-only and can't render the images well

   * While it's working perfectly for my use case, it should work well for other reasonable cases (different terminal sizes, number+name for the gallery)

Conda environment
-----------------
.. _conda-environment:

.. code-block:: sh

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

Manual installation
-------------------

.. _manual-installation:


Note: if you want to make some edits, you should install it in a conda environment. See above

.. code-block:: sh

   # Use the latest stable version (recommended for usage)
   # Make sure the version number is the latest
   git clone -b 'v0.12.1' --depth 1 https://github.com/akazukin5151/koneko.git
   # ...or Use the master branch for nightly builds
   git clone -b master https://github.com/akazukin5151/koneko.git

   # Run the tests (for those who want to edit)
   pytest testing/ -vvvv -l

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


Build and upload to PyPI
------------------------


#. Run ``pytest testing/ -vvvv -l``.
#. Review github action logs to make sure nothing is wrong
#. Run ``java -jar ~/Applications/plantuml.jar docs/puml/*/ -o render`` (change 1st argument to where `plantuml.jar <https://plantuml.com/download`_ is stored)
#. Bump version info in ``__init__.py``, ``setup.py``, and ``docs/pages/CONTRIBUTING.rst``
#. Run:

.. code-block:: sh

   rm -r dist
   python setup.py sdist bdist_wheel
   twine upload dist/*
   pip install koneko --upgrade
