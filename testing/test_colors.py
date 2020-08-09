from koneko import colors


def test_public_constants():
    assert colors.RED == '\x1b[31m'
    assert colors.MAGENTA == '\x1b[35m'
    assert colors.BLUE == '\x1b[34m'
    assert colors.RESET == '\x1b[39m'
    assert colors.BLUE_N == '\x1b[31m[\x1b[34mn\x1b[31m]\x1b[39m'


def test_letter_with_brackets():
    assert colors.n == '\x1b[31m[\x1b[35mn\x1b[31m]\x1b[39m'
    assert colors.p == '\x1b[31m[\x1b[35mp\x1b[31m]\x1b[39m'
    assert colors.r == '\x1b[31m[\x1b[35mr\x1b[31m]\x1b[39m'
    assert colors.q == '\x1b[31m[\x1b[35mq\x1b[31m]\x1b[39m'
    assert colors.m == '\x1b[31m[\x1b[35mm\x1b[31m]\x1b[39m'
    assert colors.b == '\x1b[31m[\x1b[35mb\x1b[31m]\x1b[39m'
    assert colors.o_ == '\x1b[31m[\x1b[35mo\x1b[31m]\x1b[39m'
    assert colors.d_ == '\x1b[31m[\x1b[35md\x1b[31m]\x1b[39m'
    assert colors.f == '\x1b[31m[\x1b[35mf\x1b[31m]\x1b[39m'


def test_letter_with_coords():
    assert colors.i == '\x1b[31m[\x1b[35mi\x1b[31m]\x1b[31m[\x1b[34mn\x1b[31m]\x1b[39m\x1b[39m'


def test_two_letter_with_coords():
    assert colors.a == '\x1b[31m[\x1b[35ma\x1b[39m\x1b[31m{\x1b[34mx\x1b[31m}{\x1b[34my\x1b[31m}\x1b[39m|\x1b[35mA\x1b[31m[\x1b[34mn\x1b[31m]\x1b[39m\x1b[31m]\x1b[39m'
    assert colors.o == '\x1b[31m[\x1b[35mo\x1b[39m\x1b[31m{\x1b[34mx\x1b[31m}{\x1b[34my\x1b[31m}\x1b[39m|\x1b[35mO\x1b[31m[\x1b[34mn\x1b[31m]\x1b[39m\x1b[31m]\x1b[39m'
    assert colors.d == '\x1b[31m[\x1b[35md\x1b[39m\x1b[31m{\x1b[34mx\x1b[31m}{\x1b[34my\x1b[31m}\x1b[39m|\x1b[35mD\x1b[31m[\x1b[34mn\x1b[31m]\x1b[39m\x1b[31m]\x1b[39m'


def test_base1():
    assert colors.base1 == [
        '\x1b[31m{\x1b[34mx\x1b[31m}{\x1b[34my\x1b[31m}\x1b[39m',
        ' view image at (x, y); ',
        '\x1b[31m[\x1b[35mi\x1b[31m]\x1b[31m[\x1b[34mn\x1b[31m]\x1b[39m\x1b[39m',
        ' view nth image; ',
'\x1b[31m[\x1b[35md\x1b[39m\x1b[31m{\x1b[34mx\x1b[31m}{\x1b[34my\x1b[31m}\x1b[39m|\x1b[35mD\x1b[31m[\x1b[34mn\x1b[31m]\x1b[39m\x1b[31m]\x1b[39m',
     ' download image;\n',
     '\x1b[31m[\x1b[35mo\x1b[39m\x1b[31m{\x1b[34mx\x1b[31m}{\x1b[34my\x1b[31m}\x1b[39m|\x1b[35mO\x1b[31m[\x1b[34mn\x1b[31m]\x1b[39m\x1b[31m]\x1b[39m',
    ' open image in browser; ']


def test_base2():
    assert colors.base2 == [
        '\x1b[31m[\x1b[35mn\x1b[31m]\x1b[39m',
        'ext page; ',
        '\x1b[31m[\x1b[35mp\x1b[31m]\x1b[39m',
        'revious page;\n',
        '\x1b[31m[\x1b[35mr\x1b[31m]\x1b[39m',
        'eload and re-download all; ',
        '\x1b[31m[\x1b[35mq\x1b[31m]\x1b[39m',
        'uit (with confirmation); '
    ]

