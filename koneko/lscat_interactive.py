from pathlib import Path

from koneko import KONEKODIR, lscat


class FakeData:
    def __init__(self, path):
        self.download_path = path

    @classmethod
    def gallery(cls):
        return cls(KONEKODIR / 'testgallery')

    @classmethod
    def user(cls):
        # Make sure it has a .koneko file
        return cls(KONEKODIR / 'testuser')


def main():
    print('Welcome to the lscat interactive script')
    print('1. Launch koneko configuration assistance')
    print('2. Display KONEKODIR / testgallery')
    print('3. Display KONEKODIR / testuser')
    print('4. Display a specified path')
    ans = input('\nPlease select an action:\n')

    case = {
        '1': config_assistance,
        '2': display_gallery,
        '3': display_user,
        '4': display_path
    }
    func = case.get(ans, None)
    if func:
        func()
    else:
        print('Invalid command! Exiting...')


def display_gallery():
    data = FakeData.gallery()
    lscat.show_instant(lscat.TrackDownloads, data, True)

def display_user():
    data = FakeData.user()
    lscat.show_instant(lscat.TrackDownloadsUsers, data)

def display_path():
    path = input('Please paste in your path:\n')
    if not Path(path).is_dir():
        print('Invalid path!')
        return

    data = FakeData(path)
    lscat.show_instant(lscat.TrackDownloads, data, True)


def config_assistance():
    print('todo')


if __name__ == '__main__':
    main()
