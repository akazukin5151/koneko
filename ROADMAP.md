## Roadmap

### Features

* Illust-related mode can't go back to image mode
* In-depth usage documentation; use letters to represent modes (at least in public docs) rather than numbers
    * Make sure the diagrams in HACKING.md is up-to-date

### Known bugs

* Some info (eg manual) are still being hidden by ueberzug; general unreliability of prints (need a rethink of implementation)
    * Help message that responds to terminal width
    * Consider [ucollage](https://github.com/ckardaris/ucollage/)

* In the logs, urllib3 warns that `Connection pool is full, discarding connection: i.pximg.net`. See [customising pool behaviour](https://urllib3.readthedocs.io/en/latest/advanced-usage.html#customizing-pool-behavior) from urllib3.
* Prefetch thread still running (downloading) hangs the entire app, even when user quits. Cannot use daemon threads as it still hangs then noisly aborts. Changing prompt.ask_quit() into a UI method so that it can pass a threading.Event() to downloads, doesn't work either as all the downloads has already been submitted to the ThreadPoolExecutor before the user is quick enough to send 'q'. The only way is to interrupt the urllib download process, which is going to be unsafe if you don't know what you're doing.
* There seems to be a delay between entering `koneko` and startup, but the delay is before the first line of the script even executes. Import time is fast. `pip install` using the wheel seems to reduce the delay. Directly running the script using `python koneko/main.py` or `python koneko/lscat_app.py` is faster as well. Seems like it's a delay in going to `miniconda/lib/python3.8/site-packages/koneko.egg-link` (installed with `python setup.py develop`)?

### Code

* Refine ueberzug dependency in setup.py/requirements.txt, keeping in mind that ueberzug only builds on linux
