from day20 import *

def assertReachable(p1, p2, graph):
    assert p1 in graph[p2]
    assert p2 in graph[p1]

def assertReachables(point, points, graph):
    for p in points:
        assertReachable(p, point, graph)

def test_follow_straight_line():
    path = parse("^NE$")
    print(path)
    graph = follow(path)[0]
    assertReachable((0,0), (0,1), graph)
    assertReachable((0,1), (1,1), graph)

def test_follow_branch_empty_node():
    path = parse("^N(E|)N$")
    print(path)
    graph = follow(path)[0]
    assertReachable((0,0), (0,1), graph)
    assertReachables((0,1), [(1,1), (0,2)], graph)
    assertReachable((1,1), (1,2), graph)

def test_follow_branch():
    path = parse("^N(E|W)N$")
    print(path)
    graph = follow(path)[0]
    assertReachable((0,0), (0,1), graph)
    assertReachables((0,1), [(1,1), (-1,1)], graph)
    assertReachable((-1,1), (-1,2), graph)
    assertReachable((1,1), (1,2), graph)

def test_follow_square():
    path = parse("^NESW$")
    print(path)
    graph = follow(path)[0]
    assertReachable((0,0), (0,1), graph)
    assertReachable((0,1), (1,1), graph)
    assertReachable((1,1), (1,0), graph)
    assertReachable((1,0), (0,0), graph)

def test_get_dists():
    graph = follow(parse("^N(E|W)N$"))[0]
    dists = get_dists(graph, Point(0,0))
    assert dists[(0, 0)] == 0
    assert dists[(0, 1)] == 1
    assert dists[(1, 1)] == 2
    assert dists[(-1, 1)] == 2
    assert dists[(1, 2)] == 3
    assert dists[(-1, 2)] == 3

def test_farthest_room():
    graph = follow(parse("^WNE$"))[0]
    assert farthest_room(graph, (0,0)) == (Point(0,1), 3)

def test_farthest_medium():
    graph = follow(parse("^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"))[0]
    assert farthest_room(graph, (0,0))[1] == 31
