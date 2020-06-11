from blessed import Terminal
from koneko import utils, lscat

TERM = Terminal()

def _width_paddingx() -> int:
    settings = utils.get_config_section('lscat')
    return (
        settings.map(lambda s: s.getint('image_width', fallback=18)).value_or(18),
        settings.map(lambda s: s.getint('images_x_spacing', fallback=2)).value_or(2)
    )

def ncols_config():
    return lscat.ncols(TERM.width, *_width_paddingx())

def xcoords_config(offset=0):
    return lscat.xcoords(TERM.width, *_width_paddingx(), offset)

def ycoords_config():
    settings = utils.get_config_section('lscat')
    img_height = settings.map(
        lambda s: s.getint('image_height', fallback=8)
    ).value_or(8)
    paddingy = settings.map(
        lambda s: s.getint('images_y_spacing', fallback=1)
    ).value_or(1)
    return lscat.ycoords(TERM.height, img_height, paddingy)

def gallery_page_spacing_config():
    settings = utils.get_config_section('lscat')
    return settings.map(
        lambda s: s.getint('gallery_page_spacing', fallback=23)
    ).value_or(23)

def users_page_spacing_config():
    settings = utils.get_config_section('lscat')
    return settings.map(
        lambda s: s.getint('users_page_spacing', fallback=22)
    ).value_or(22)

def thumbnail_size_config():
    settings = utils.get_config_section('lscat')
    return settings.map(
        lambda s: s.getint('image_thumbnail_size', fallback=310)
    ).value_or(310)

def get_gen_users_settings():
    settings = utils.get_config_section('lscat')
    return (
        settings.map(
            lambda s: s.getint('users_print_name_xcoord', fallback=18)
        ).value_or(18),
        settings.map(lambda s: s.getint('images_x_spacing', fallback=2)).value_or(2)
    )
