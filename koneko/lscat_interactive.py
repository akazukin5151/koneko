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
    print('Welcome to lscat')
    print('1. Launch koneko configuration assistance')
    print('2. Display KONEKODIR / testgallery')
    print('3. Display KONEKODIR / testuser')
    print('4. Display a specified path')
    ans = input('\nPlease select an action:\n')

    case = {
        '1': lambda x: print('Not implemented yet!'),
        '2': display_test,
        '3': display_test,
        '4': display_path
    }
    func = case.get(ans, None)
    if func:
        func(ans)
    else:
        print('Invalid command! Exiting...')


def display_test(ans):
    if ans == '2':
        data = FakeData.gallery()
        lscat.show_instant(lscat.TrackDownloads, data)
    else:
        data = FakeData.user()
        lscat.show_instant(lscat.TrackDownloadsUsers, data)

def display_path(ans):

    path = input('Please paste in your path:\n')
    if not Path(path).is_dir():
        print('Invalid path!')
        return

    data = FakeData(path)
    lscat.show_instant(lscat.TrackDownloads, data)


if __name__ == '__main__':
    main()
