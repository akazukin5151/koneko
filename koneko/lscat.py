"""The default image renderer for koneko.

1) The ui classes start the download with the appropriate tracker instance.
   The tracker's update method acts as a callback upon a finished download
2) After each image finishes downloading, the callback is triggered (`tracker.update()`)
3) The `update()` method stores which images have finished downloading and their
   respective number, but has not been displayed yet.
4) On every callback, the `update()` method inspects the list against the given order.
   If the next image to be shown is in the list, it is displayed.
5) Immediately, remove the image from the lists and repeat the `_inspect()` method
   for the next valid image. If there is one, repeat. If not, do nothing and wait for
   more completed downloads

TLDR if you want to write your own renderer (with icat or not), the API is:
    - Provide a `tracker` with an `update()` method that receives completed downloads
    - Provide a generator that receives images to display in order, from the tracker
"""

import os
import threading
from abc import ABC

from pixcat import Image
from returns.result import safe

from koneko import (
    pure,
    TERM,
    utils,
    files,
    config,
    printer,
    KONEKODIR,
    WELCOME_IMAGE
)


def show_single(x: int, y: int, thumbnail_size: int) -> 'IO[Image]':
    if config.use_ueberzug():
        return ueberzug_display(WELCOME_IMAGE, x, y, thumbnail_size / 20)

    img = Image(WELCOME_IMAGE).thumbnail(thumbnail_size)
    img.show(align='left', x=x, y=y)
    return img


def show_single_x(x: int, thumbnail_size: int) -> 'IO[Image]':
    return show_single(x, 0, thumbnail_size)


def show_single_y(y: int, thumbnail_size: int) -> 'IO[Image]':
    # Default usage of config module
    return show_single(config.xcoords_config()[1], y, thumbnail_size)


def show_instant_sample(thumbnail_size, xpadding, image_width: int) -> 'IO':
    xcoords = pure.xcoords(TERM.width, image_width, xpadding)
    if not config.use_ueberzug():
        for x in xcoords:
            show_single(x, 0, thumbnail_size)
        return None

    return display_multiple_x_ueberzug(WELCOME_IMAGE, xcoords, thumbnail_size)


def display_user_row(size, padding: int, preview_xcoords: 'list[int]') -> 'IO':
    canvas = show_single(padding, 0, size)
    if not config.use_ueberzug():
        for px in preview_xcoords:
            show_single_x(px, size)
        return None

    display_multiple_x_canvas(canvas, preview_xcoords, WELCOME_IMAGE, size)
    return canvas


def display_multiple_x_ueberzug(path, xcoords, thumbnail_size):
    ueberzug = utils.try_import_ueberzug()
    canvas = ueberzug.Canvas()
    canvas.__enter__()

    display_multiple_x_canvas(canvas, xcoords, path, thumbnail_size)
    return canvas


def display_multiple_x_canvas(canvas, xcoords, path, thumbnail_size):
    scaler = utils.try_get_FIT_CONTAIN()
    vis = utils.try_get_VISIBLE()
    for x in xcoords:
        canvas.create_placement(
            str(path) + str(x),  # Identifier must be unique
            path=str(path),
            x=x,
            y=0,
            width=thumbnail_size / 20,
            height=thumbnail_size / 20,
            scaler=scaler,
            visibility=vis,
        )


def hide_if_exist(image: Image) -> 'IO':
    if image:
        try:
            image.hide()
        except AttributeError:
            image.__exit__()
        else:
            printer.move_cursor_up(1)


def icat(path: str) -> 'IO':
    """icat (system command) and pixcat behaves differently.
    pixcat prints out the escape codes, shifting the current cursor position,
    but calling a system command does not. I abuse this fact in the main
    generators to make printing pages easier, and user mode is possible only
    because of this fact.
    """
    Image(path).show()


def ueberzug_center_align(path):
    """Display center-aligned image in original size"""
    ueberzug = utils.try_import_ueberzug()
    canvas = ueberzug.Canvas()
    canvas.__enter__()
    canvas.create_placement(
        str(path),
        path=str(path),
        x=config.ueberzug_center_spaces(),
        y=0,
        visibility=utils.try_get_VISIBLE()
    )
    return canvas


def ueberzug_display(path, x, y, size):
    """Without any setup, display given path with variable coordinates and size"""
    ueberzug = utils.try_import_ueberzug()
    canvas = ueberzug.Canvas()
    canvas.__enter__()
    canvas.create_placement(
        str(path),
        path=str(path),
        x=x,
        y=y,
        width=size,
        height=size,
        visibility=utils.try_get_VISIBLE()
    )
    return canvas


def display_canvas(canvas, img_path, x, y, size):
    """Given independently instantiated ueberzug canvas, display image"""
    canvas.create_placement(
        str(img_path),
        path=str(img_path),
        x=x,
        y=y,
        width=size,
        height=size,
        scaler=utils.try_get_FIT_CONTAIN(),
        visibility=utils.try_get_VISIBLE(),
    )


def handle_scroll(cls, data, myslice):
    tracker = cls(data)
    tracker.orders = tracker.orders[myslice]
    for x in sorted(os.listdir(data.download_path)):
        if not x.startswith('.'):
            tracker.update(x)
    return tracker.canvas


def show_instant(cls: 'lscat.<class>', data: 'data.<class>') -> 'IO':
    tracker = cls(data)
    # Filter out invisible files
    # (used to save splitpoint and total_imgs without requesting)
    for x in os.listdir(data.download_path):
        if not x.startswith('.'):
            tracker.update(x)

    if isinstance(cls, TrackDownloads) and config.check_print_info():
        number_of_cols = config.ncols_config()
        spacings = config.gallery_print_spacing_config()
        printer.print_cols(spacings, number_of_cols)
        print('\n')


class AbstractTracker(ABC):
    def __init__(self):
        # Defined in child classes
        self.orders: 'list[int]'
        self.generator: 'generator[str]'

        self._lock = threading.Lock()
        self._downloaded: 'list[str]' = []
        self._numlist: 'list[int]' = []

        self.generator.send(None)

    def update(self, new: str) -> 'IO':
        # Can't use queues/channels instead of a lock, because of race conditions
        with self._lock:
            self._downloaded.append(new)
            self._numlist.append(int(new[:3]))

        self._inspect()

    def _inspect(self) -> 'IO':
        """Inspect the list of images that have finished downloading but not displayed
        yet. According to the given orders list, if the next image to be displayed
        is in the list, display it, then look for the next next image and repeat.
        """
        if not self.orders:
            return

        next_num = self.orders[0]

        if next_num in self._numlist:
            pic = self._downloaded[self._numlist.index(next_num)]
            self.generator.send(pic)

            self.orders = self.orders[1:]
            self._downloaded.remove(pic)
            self._numlist.remove(next_num)
            if self._downloaded and self.orders:
                self._inspect()


class TrackDownloads(AbstractTracker):
    """For gallery modes (1 & 5)"""
    def __init__(self, data: 'data.<class>'):
        self.orders = list(range(30))
        if config.use_ueberzug():
            ueberzug = utils.try_import_ueberzug()
            self.canvas = ueberzug.Canvas()
            self.generator = generate_page_ueberzug(data.download_path, self.canvas)
        else:
            self.canvas = None
            self.generator = generate_page(data.download_path)
        super().__init__()


class TrackDownloadsUsers(AbstractTracker):
    """For user modes (3 & 4)"""
    def __init__(self, data: 'data.<class>'):
        print_info = config.check_print_info()

        # Tries to access splitpoint attribute in the data instance
        # If it fails, `fix` it by calling the read_invis() function
        # Either way, the Success() result is inside the Result[] monad, so unwrap() it
        safe_func: 'func[Result[int]]' = safe(lambda: data.splitpoint)
        splitpoint: int = safe_func().fix(lambda x: files.read_invis(data)).unwrap()

        # splitpoint == number of artists
        # Each artist has 3 previews, so the total number of pics is
        # splitpoint * 3 + splitpoint == splitpoint * 4
        self.orders = pure.generate_orders(splitpoint * 4, splitpoint)

        if config.use_ueberzug():
            ueberzug = utils.try_import_ueberzug()
            self.canvas = ueberzug.Canvas()
            self.generator = generate_users_ueberzug(data.download_path, self.canvas, print_info)
        else:
            self.canvas = None
            self.generator = generate_users(data.download_path, print_info)
        super().__init__()


def generate_page(path: 'Path') -> 'IO':
    """Given number, calculate its coordinates and display it, then yield"""
    left_shifts = config.xcoords_config()
    rowspaces = config.ycoords_config()
    number_of_cols = config.ncols_config()
    number_of_rows = config.nrows_config()
    page_spacing = config.gallery_page_spacing_config()
    thumbnail_size = config.thumbnail_size_config()

    os.system('clear')
    while True:
        # Release control. When _inspect() sends another image,
        # assign to the variables and display it again
        image = yield

        number = int(image.split('_')[0])
        x = number % number_of_cols
        y = number // number_of_cols

        if number % (number_of_cols * number_of_rows) == 0 and number != 0:
            print('\n' * page_spacing)

        Image(path / image).thumbnail(thumbnail_size).show(
            align='left', x=left_shifts[x], y=rowspaces[(y % number_of_rows)]
        )


def generate_users(path: 'Path', print_info=True) -> 'IO':
    preview_xcoords = config.xcoords_config(offset=1)[-3:]
    message_xcoord, padding = config.get_gen_users_settings()
    page_spacing = config.users_page_spacing_config()
    thumbnail_size = config.thumbnail_size_config()

    os.system('clear')
    while True:
        # Wait for artist pic
        a_img = yield
        artist_name = a_img.split('.')[0].split('_')[-1]
        number = a_img.split('_')[0][1:]

        if print_info:
            print(' ' * message_xcoord, number, '\n',
                  ' ' * message_xcoord, artist_name,
                  sep='')
        print('\n' * page_spacing)  # Scroll to new 'page'

        # Display artist profile pic
        Image(path / a_img).thumbnail(thumbnail_size).show(align='left', x=padding, y=0)

        # Display the three previews
        for i in range(3):
            Image(path / (yield)).thumbnail(thumbnail_size).show(
                align='left', y=0, x=preview_xcoords[i]
            )


class TrackDownloadsImage(AbstractTracker):
    """Experimental"""
    def __init__(self, data):
        min_num = data.page_num + 1
        self.orders = list(range(min_num, 30))
        if config.use_ueberzug():
            ueberzug = utils.try_import_ueberzug()
            self.canvas = ueberzug.Canvas()
            self.generator = generate_previews_ueberzug(data.download_path, min_num, self.canvas)
        else:
            self.canvas = None
            self.generator = generate_previews(data.download_path, min_num)
        super().__init__()

    def update(self, new: str):
        """Overrides base class because numlist is different"""
        self.generator.send(new)


def generate_previews(path: 'Path', min_num: int) -> 'IO':
    """Experimental"""
    rowspaces = config.ycoords_config()
    left_shifts = config.xcoords_config()
    _xcoords = (left_shifts[0], left_shifts[-1])
    thumbnail_size = config.thumbnail_size_config()

    i = 0
    while True:
        image = yield
        i += 1

        number = int(image.split('_')[1].replace('p', '')) - min_num
        y = number % 2
        if i <= 2:
            x = 0
        else:
            x = 1

        Image(path / image).thumbnail(thumbnail_size).show(
            align='left', x=_xcoords[x], y=rowspaces[y]
        )



def generate_page_ueberzug(path: 'Path', canvas) -> 'IO':
    left_shifts = config.xcoords_config()
    rowspaces = config.ycoords_config()
    number_of_cols = config.ncols_config()
    number_of_rows = config.nrows_config()
    thumbnail_size = config.thumbnail_size_config()
    size = thumbnail_size / 20

    os.system('clear')
    canvas.__enter__()
    while True:
        image = yield

        number = int(image.split('_')[0])
        x = number % number_of_cols
        y = number // number_of_cols

        display_canvas(
            canvas,
            path / image,
            left_shifts[x],
            rowspaces[y % number_of_rows],
            size
        )


def generate_users_ueberzug(path: 'Path', canvas, print_info=True) -> 'IO':
    preview_xcoords = config.xcoords_config(offset=1)[-3:]
    message_xcoord, padding = config.get_gen_users_settings()
    page_spacing = config.users_page_spacing_config()
    thumbnail_size = config.thumbnail_size_config()
    size = thumbnail_size / 20

    number_of_cols = config.ncols_config()
    number_of_rows = config.nrows_config()
    rowspaces = config.ycoords_config()
    msg_rows = [rowspaces[0]] + [rowspaces[1]] * (number_of_rows - 1)

    os.system('clear')
    canvas.__enter__()
    for row in range(number_of_rows):
        ycoord = row % number_of_rows
        a_img = yield
        artist_name = a_img.split('.')[0].split('_')[-1]
        number = a_img.split('_')[0][1:]

        if print_info:
            print(
                '\n' * msg_rows[ycoord],
                ' ' * message_xcoord, number, '\n',
                ' ' * message_xcoord, artist_name,
                sep=''
            )

        # Display artist profile pic
        display_canvas(
            canvas,
            path / a_img,
            padding,
            rowspaces[ycoord],
            size
        )

        # Display the three previews
        for j in range(3):
            display_canvas(
                canvas,
                path / (yield),
                preview_xcoords[j],
                rowspaces[ycoord],
                size
            )

    while True:  # Prevent StopIteration errors
        yield


def generate_previews_ueberzug(path: 'Path', min_num: int, canvas) -> 'IO':
    rowspaces = config.ycoords_config()
    left_shifts = config.xcoords_config()
    _xcoords = (left_shifts[0], left_shifts[-1])
    thumbnail_size = config.thumbnail_size_config()
    size = thumbnail_size / 20

    canvas.__enter__()
    for preview_num in range(4):  # Max 4 previews
        image = yield

        number = int(image.split('_')[1].replace('p', '')) - min_num
        y = number % 2
        if preview_num < 2:
            x = 0
        else:
            x = 1

        display_canvas(
            canvas,
            path / image,
            _xcoords[x],
            rowspaces[y],
            size
        )

    while True:  # Prevent StopIteration errors
        yield
