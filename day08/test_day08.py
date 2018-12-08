from day08 import *
import pytest

@pytest.fixture
def data():
    return [int(s) for s in "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2".split(" ")]

def test_ans1(data):
    assert ans(data)[0] == 138

def test_ans2(data):
    assert ans(data)[1] == 66
