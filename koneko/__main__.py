import os
import sys

from koneko import api, cli, main, utils, config


def _main() -> 'IO':
    """Read config file, start login, process any cli arguments, go to main loop"""
    if not (args := cli.handle_vh()):
        sys.exit(0)

    credentials, your_id = config.begin_config()

    utils.handle_missing_pics()

    api.myapi.start(credentials)

    if len(sys.argv) != 1:
        func = cli.launch_mode
    else:
        func = main.main_loop

    while True:
        try:
            func(args, your_id)
        except KeyboardInterrupt:
            pass

        os.system('clear')
        main.main_loop(None, your_id)


if __name__ == '__main__':
    _main()
