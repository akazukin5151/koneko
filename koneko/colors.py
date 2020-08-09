"""Export the colors for [h]elp screen keys. Functions are pure"""

# Public constants
RED = '\x1b[31m'
MAGENTA = '\x1b[35m'
BLUE = '\x1b[34m'
RESET = '\x1b[39m'

# Private constants
_RED_BRACKET_OPEN = ''.join((RED, '['))
_RED_BRACKET_CLOSE = ''.join((RED, ']'))

# Public constant
BLUE_N = ''.join([_RED_BRACKET_OPEN, BLUE, 'n', _RED_BRACKET_CLOSE, RESET])


# Private
def _letter_with_brackets(letter: str) -> str:
    """[] are red and a is magenta
    >>> _letter_with_brackets("a")
    ... [a]
    """
    return ''.join([_RED_BRACKET_OPEN, MAGENTA, letter, _RED_BRACKET_CLOSE, RESET])


def _letter_with_coords(letter: str) -> str:
    """letter is magenta, n is blue, [] is red
    >>> _letter_with_coords("i")
    ... [i][n]
    """
    return ''.join([_RED_BRACKET_OPEN, MAGENTA, letter, _RED_BRACKET_CLOSE, BLUE_N, RESET])


def _two_letter_with_coords(letter: str) -> str:
    """[] and {} is red, | is black, o and O is magenta, y and x is blue
    >>> _two_letter_with_coords("o")
    ... [o{y}{x}|O[n]]
    """
    return ''.join([
        _RED_BRACKET_OPEN,
        MAGENTA,
        letter.lower(),
        RESET,
        _COORDS,
        '|',
        MAGENTA,
        letter.upper(),
        BLUE_N,
        _RED_BRACKET_CLOSE,
        RESET,
    ])


_letters = ['n', 'p', 'r', 'q', 'm', 'b', 'o', 'd', 'f']
_tlc = ['a', 'o', 'd']
# {y}{x}
_COORDS = ''.join([RED, '{', BLUE, 'x', RED, '}{', BLUE, 'y', RED, '}', RESET])


# Public constants
n, p, r, q, m, b, o_, d_, f = [_letter_with_brackets(letter) for letter in _letters]

i = _letter_with_coords('i')

a, o, d = [_two_letter_with_coords(letter) for letter in _tlc]

# For galleries
base1 = [
    _COORDS, ' view image at (x, y); ',
    i, ' view nth image; ',
    d, ' download image;\n',
    o, ' open image in browser; '
]

base2 = [
    n, 'ext page; ',
    p, 'revious page;\n',
    r, 'eload and re-download all; ',
    q, 'uit (with confirmation); ',
]
