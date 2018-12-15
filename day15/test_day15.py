import pytest
from textwrap import dedent
from day15 import *

@pytest.fixture
def grid1():
    return parse(dedent("""\
    ...
    .#.
    ..."""))

@pytest.fixture
def grid2():
    return parse(dedent("""\
    .G.
    .#.
    ..."""))

@pytest.fixture
def grid3():
    return parse(dedent("""\
    .G.
    .#.
    .E."""))

def test_sort_points():
    points = [Point(1,1), Point(0,1), Point(1,0), Point(0,0)]
    assert sorted(points, key=sort_points) == [Point(0,0), Point(1,0), Point(0,1), Point(1,1)]

def test_distance(grid1):
    assert distance(grid1, Point(0,0), Point(0,0)) == 0
    assert distance(grid1, Point(0,0), Point(1,0)) == 1
    assert distance(grid1, Point(0,0), Point(0,1)) == 1
    assert distance(grid1, Point(0,0), Point(2,2)) == 4

def test_distance_barrier(grid2):
    assert distance(grid2, Point(0,0), Point(2,0)) == 6

def test_distance_unreachable(grid3):
    assert distance(grid3, Point(0,0), Point(2,0)) == None

def test_choose_next_step(grid1):
    assert choose_next_step(grid1, Point(1,0), Point(1,2)) == Point(0,0)

def test_choose_next_step_unreachable(grid3):
    assert choose_next_step(grid3, Point(0,0), Point(2,0)) == None
