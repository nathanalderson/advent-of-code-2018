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

@pytest.fixture
def grid4():
    return parse(dedent("""\
    #######
    #E..G.#
    #...#.#
    #.G.#G#
    #######"""))

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
    assert choose_next_step(grid1, Point(1,0), Point(2,0)) == Point(2,0)
    grid = parse(dedent("""\
    #######
    #...G.#
    #..G.G#
    #.#.#G#
    #...#E#
    #.....#
    #######"""))
    assert choose_next_step(grid, Point(3,2), Point(5,5)) == Point(3,3)

def test_choose_next_step_unreachable(grid3):
    assert choose_next_step(grid3, Point(0,0), Point(2,0)) == None

def test_find_targets(grid4):
    targets = find_targets(grid4, Unit(Grid.ELF,0,0))
    assert sorted(targets.keys(), key=sort_points) == [Point(4,1), Point(2,3), Point(5,3)]

def test_get_in_range(grid4):
    targets = find_targets(grid4, Unit(Grid.ELF,0,0))
    in_range = get_in_range(grid4, Point(0,0), targets)
    assert len(in_range) == 6
    assert Point(3,1) in in_range

# def test_get_in_range2():
#     targets = {Point(x=5, y=2): Unit(type='G', hp=200, attack=3), Point(x=5, y=3): Unit(type='G', hp=200, attack=3), Point(x=3, y=4): Unit(type='G', hp=200, attack=3), Point(x=3, y=1): Unit(type='G', hp=200, attack=3)}
#     in_range = get_in_range(targets)
#     assert Point(3,2) not in in_range

def test_play_round(grid4):
    all_done = play_round(grid4)
    assert not all_done
    assert str(grid4) == dedent("""\
    #######
    #.EG..#
    #.G.#.#
    #...#G#
    #######
    """)

def test_ans1():
    grid = parse(dedent("""\
    #######
    #.G...#
    #...EG#
    #.#.#G#
    #..G#E#
    #.....#
    #######"""))
    assert ans1(grid) == 27730

def test_ans1_2():
    grid = parse(dedent("""\
    #######
    #G..#E#
    #E#E.E#
    #G.##.#
    #...#E#
    #...E.#
    #######"""))
    assert ans1(grid) == 36334

def test_ans1_3():
    grid = parse(dedent("""\
    #########
    #G......#
    #.E.#...#
    #..##..G#
    #...##..#
    #...#...#
    #.G...G.#
    #.....G.#
    #########"""))
    assert ans1(grid) == 18740

def test_ans1_4():
    grid = parse(dedent("""\
    #######
    #.E...#
    #.#..G#
    #.###.#
    #E#G#G#
    #...#G#
    #######"""))
    assert ans1(grid) == 28944

def test_ans1_5():
    grid = parse(dedent("""\
    #######
    #E.G#.#
    #.#G..#
    #G.#.G#
    #G..#.#
    #...E.#
    #######"""))
    assert ans1(grid) == 27755

def test_ans1_6():
    grid = parse(dedent("""\
    #######
    #E..EG#
    #.#G.E#
    #E.##E#
    #G..#.#
    #..E#.#
    #######"""))
    assert ans1(grid) == 39514
