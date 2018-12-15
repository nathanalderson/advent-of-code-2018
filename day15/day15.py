"""
Several algorithms and datastructures from https://www.redblobgames.com/pathfinding/a-star/implementation.html
"""
from collections import namedtuple
from typing import Optional, List, Dict, Tuple, TypeVar, Generic
import heapq

Unit = namedtuple('Unit', 'type hp attack')

Point = namedtuple('Point', 'x y')
def sort_points(p: Point) -> Tuple[int, int]:
    """sort points in reading order"""
    return (p.y, p.x)

T = TypeVar('T')
class PriorityQueue(Generic[T]):
    def __init__(self):
        self.elements: List[T] = []

    def empty(self) -> int:
        return len(self.elements) == 0

    def put(self, item, priority):
        heapq.heappush(self.elements, (priority, item))

    def get(self) -> T:
        return heapq.heappop(self.elements)[1]

class Grid:
    WALL = '#'
    ELF = 'E'
    GOBLIN = 'G'
    EMPTY = '.'

    def __init__(self, width, height):
        self.width = width
        self.height = height
        self.walls: Set[Point] = set()
        self.units: Dict[Point, Unit] = {}

    def in_bounds(self, point):
        (x, y) = point
        return 0 <= x < self.width and 0 <= y < self.height

    def passable(self, point):
        return point not in self.walls and point not in self.units

    def cost(self, from_point, to_point):
        return 1

    def neighbors(self, point):
        (x, y) = point
        results = [Point(x+1, y), Point(x, y-1), Point(x-1, y), Point(x, y+1)]
        # if (x + y) % 2 == 0: results.reverse() # aesthetics
        results = filter(self.in_bounds, results)
        results = filter(self.passable, results)
        return results

    def __str__(self):
        def chars():
            for y in range(0, self.height):
                for x in range(0, self.width):
                    yield self.objects.get(Point(x,y), Grid.EMPTY)
                yield '\n'
        return "".join(chars())

def heuristic(a, b):
    (x1, y1) = a
    (x2, y2) = b
    return abs(x1 - x2) + abs(y1 - y2)

def a_star_search(graph, start, goal):
    frontier = PriorityQueue()
    frontier.put(start, 0)
    came_from: Dict[Point, Optional[Point]] = {start: None}
    cost_so_far: Dict[Point, int] = {start: 0}
    while not frontier.empty():
        current = frontier.get()
        if current == goal:
            break
        for n in graph.neighbors(current):
            new_cost = cost_so_far[current] + graph.cost(current, n)
            if n not in cost_so_far or new_cost < cost_so_far[n]:
                cost_so_far[n] = new_cost
                priority = new_cost + heuristic(goal, n)
                frontier.put(n, priority)
                came_from[n] = current
    return came_from, cost_so_far

def distance(grid: Grid, start: Point, goal: Point) -> Optional[int]:
    came_from, cost_so_far = a_star_search(grid, start, goal)
    return cost_so_far.get(goal)

def choose_next_step(grid: Grid, start: Point, goal: Point) -> Optional[Point]:
    neighbor_dists = [(n, distance(grid, n, goal)) for n in grid.neighbors(start)]
    min_so_far = None
    mins = []
    for n, dist in neighbor_dists:
        if dist and (min_so_far is None or dist <= min_so_far):
            min_so_far = dist
            mins.append(n)
    if len(mins) > 0:
        mins.sort(key=sort_points)
        return mins[0]
    else:
        return None

def parse(s: str) -> Grid:
    lines = s.splitlines()
    height, width = len(lines), len(lines[0])
    grid = Grid(width, height)
    for y,line in enumerate(lines):
        for x,c in enumerate(line):
            if c == Grid.WALL:
                grid.walls.add(Point(x,y))
            elif c == Grid.GOBLIN or c == Grid.ELF:
                grid.units[Point(x,y)] = Unit(c, 200, 3)
    return grid

def get_play_order(grid: Grid) -> List[Point]:
    return sorted((p for p,c in grid.objects.items() if (c==Grid.ELF or c==Grid.GOBLIN)), key=sort_points)

def round(grid: Grid) -> Grid:
    play_order = get_play_order(grid)

def main():
    with open("input.txt") as f:
        input = f.read()
    grid = parse(input)

if __name__ == "__main__":
    main()