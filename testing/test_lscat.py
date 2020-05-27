import sys

import pytest

# Lmao python
sys.path.append('../koneko/koneko')
sys.path.append('testing')

from koneko import lscat

def test_xcoords():
    assert lscat.xcoords(100) == [2, 20, 38, 56, 74]

def test_icat():
    assert lscat.icat("testing/files/04_祝！！！.jpg") == None

