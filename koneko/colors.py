"""Export the colors for [h]elp screen keys"""

from colorama import Fore


# Private
def _letter(letter):
    """[] are red and a is magenta
    >>> _letter("a")
    ... [a]
    """
    return ''.join([Fore.RED, '[', Fore.MAGENTA, letter, Fore.RED, ']', Fore.RESET])

def _letter_with_coords(letter):
    """ letter is magenta, n is blue, [] is red
    >>> _letter_with_coords("i")
    ... [i][n]
    """
    return ''.join([Fore.RED, '[', Fore.MAGENTA, letter, Fore.RED, ']',
                    blue_n, Fore.RESET])

def _two_letter_with_coords(letter):
    """ [] and {} is red, | is black, o and O is magenta, y and x is blue
    >>> _two_letter_with_coords("o")
    ... [o{y}{x}|O[n]]
    """
    return ''.join([Fore.RED, '[', Fore.MAGENTA, letter.lower(), Fore.RESET, coords, '|',
                    Fore.MAGENTA, letter.upper(), blue_n, Fore.RED, ']', Fore.RESET])


_letters = ['n', 'p', 'r', 'q', 'm', 'b', 'o', 'd', 'f']
_tlc = ['a', 'o', 'd']

# Public
blue_n = ''.join([Fore.RED, '[', Fore.BLUE, 'n', Fore.RED, ']', Fore.RESET])
# {y}{x}
coords = ''.join([Fore.RED, '{', Fore.BLUE, 'x', Fore.RED, '}{', Fore.BLUE,
                  'y', Fore.RED, '}', Fore.RESET])

n, p, r, q, m, b, o_, d_, f = list(map(_letter, _letters))

i = _letter_with_coords('i')

a, o, d = list(map(_two_letter_with_coords, _tlc))

# For galleries
base1 = [
    coords, ' view image at (x, y); ',
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
