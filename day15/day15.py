"""
Several algorithms and datastructures from https://www.redblobgames.com/pathfinding/a-star/implementation.html
"""
from collections import namedtuple
from typing import Optional, Iterable, Set, List, Dict, Tuple, TypeVar, Generic
import heapq
import itertools

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
        return list(results)

    def __str__(self):
        def chars():
            for y in range(0, self.height):
                for x in range(0, self.width):
                    p = Point(x, y)
                    if p in self.walls:
                        yield '#'
                    elif p in self.units:
                        yield self.units[p].type
                    else:
                        yield Grid.EMPTY
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

def nearest_of(grid: Grid, point: Point, points: Iterable[Point]) -> Optional[Point]:
    dists = [(p, distance(grid, point, p)) for p in points]
    min_so_far = None
    mins = []
    for p, dist in dists:
        if dist and (min_so_far is None or dist <= min_so_far):
            min_so_far = dist
            mins.append(p)
    if len(mins) > 0:
        mins.sort(key=sort_points)
        return mins[0]
    else:
        return None

def choose_next_step(grid: Grid, start: Point, goal: Point) -> Optional[Point]:
    neighbors = grid.neighbors(start)
    # print("neighbors", neighbors)
    if goal in neighbors:
        return goal
    else:
        return nearest_of(grid, goal, neighbors)

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
    return sorted((p for p,c in grid.units.items()), key=sort_points)

def find_targets(grid: Grid, unit: Unit) -> Dict[Point, Unit]:
    # print("find_targets")
    # print("    ", grid.units)
    return {p: u for p, u in grid.units.items() if u.type != unit.type}

def get_in_range(grid: Grid, targets: Dict[Point,Unit]) -> Set[Point]:
    # print("get_in_range")
    # print("    targets = ", targets)
    neighbors = flatten(grid.neighbors(p) for p in targets.keys())
    return set(neighbors)

def flatten(l):
    return list(itertools.chain.from_iterable(l))

def play_round(grid: Grid) -> bool:
    play_order = get_play_order(grid)
    all_done = False
    while play_order:
        pos = play_order.pop(0)
        unit = grid.units.get(pos)
        # print(f"~~~~~~ {pos} {unit}")
        if not unit:
            continue # it must've died since the start of the turn
        targets = find_targets(grid, unit)
        # print(f"targets: {targets}")
        if not targets:
            all_done = True
            break
        in_range = get_in_range(grid, targets)
        # print(f"in range: {in_range}")
        if pos not in in_range:
            dest = nearest_of(grid, pos, in_range)
            # print(f"dest: {dest}")
            if dest:
                next_step = choose_next_step(grid, pos, dest)
                # print(f"next step: {next_step}")
                # print(f"moving {unit.type} from {pos} to {next_step}")
                del grid.units[pos]
                grid.units[next_step] = unit
                pos = next_step

        if pos in in_range:
            target_pos = sorted(targets.keys(), key=sort_points)[0]
            # print(f"attacking {target_pos}")
            target_unit = grid.units[target_pos]
            updated_unit = Unit(target_unit.type, target_unit.hp-unit.attack, target_unit.attack)
            if updated_unit.hp <= 0:
                del grid.units[target_pos]
            else:
                grid.units[target_pos] = updated_unit
    return all_done

def get_total_hp(grid) -> int:
    return sum(u.hp for u in grid.units.values())

def ans1(grid: Grid) -> int:
    for i in itertools.count():
        all_done = play_round(grid)
        if all_done:
            break
    complete_rounds = i
    remaining_hp = get_total_hp(grid)
    return complete_rounds * remaining_hp

def main():
    with open("input.txt") as f:
        input = f.read()
    grid = parse(input)
    print(f"ans1 = {ans1(grid)}")


if __name__ == "__main__":
    main()