"""
Several algorithms and datastructures from https://www.redblobgames.com/pathfinding/a-star/implementation.html
"""
from collections import namedtuple
from typing import Optional, Iterable, Set, List, Dict, Tuple, TypeVar, Generic
import heapq
import itertools

Unit = namedtuple('Unit', 'type hp attack')
Point = namedtuple('Point', 'x y')
class ElfDeath(Exception): pass

def sort_points(p: Point) -> Tuple[int, int]:
    """sort points in reading order"""
    return (p.y, p.x)

T = TypeVar('T')
class PriorityQueue(Generic[T]):
    def __init__(self):
        self.elements = []

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
        self.walls = set()
        self.units = {}

    def in_bounds(self, point):
        (x, y) = point
        return 0 <= x < self.width and 0 <= y < self.height

    def passable(self, point):
        return point not in self.walls and point not in self.units

    def cost(self, from_point, to_point):
        return 1

    def neighbors(self, point, allow=None):
        (x, y) = point
        results = [Point(x+1, y), Point(x, y-1), Point(x-1, y), Point(x, y+1)]
        return [p for p in results if p==allow or (self.in_bounds(p) and self.passable(p))]

    def __str__(self):
        def chars():
            for y in range(0, self.height):
                for x in range(0, self.width):
                    p = Point(x, y)
                    if p in self.walls:
                        yield Grid.WALL
                    elif p in self.units:
                        yield self.units[p].type
                    else:
                        yield Grid.EMPTY
                yield "   "
                for x in range(0, self.width):
                    p = Point(x, y)
                    if p in self.units:
                        unit = self.units[p]
                        yield f"{unit.type}({unit.hp}) "
                yield '\n'
        return "".join(chars())

def heuristic(a, b):
    (x1, y1) = a
    (x2, y2) = b
    return abs(x1 - x2) + abs(y1 - y2)

def a_star_search(graph, start, goal):
    frontier = PriorityQueue()
    frontier.put(start, 0)
    came_from = {start: None}
    cost_so_far = {start: 0}
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
    dists = [(p,d) for p,d in dists if d]
    if dists:
        min_dist = min(d for p,d in dists)
        nearests = (p for p, d in dists if d == min_dist)
        return sorted(nearests, key=sort_points)[0]
    else:
        return None

def choose_next_step(grid: Grid, start: Point, goal: Point) -> Optional[Point]:
    neighbors = grid.neighbors(start)
    if goal in neighbors:
        return goal
    else:
        return nearest_of(grid, goal, neighbors)

def parse(s: str, elf_power: int = 3) -> Grid:
    lines = s.splitlines()
    height, width = len(lines), len(lines[0])
    grid = Grid(width, height)
    for y,line in enumerate(lines):
        for x,c in enumerate(line):
            if c == Grid.WALL:
                grid.walls.add(Point(x,y))
            elif c == Grid.GOBLIN:
                grid.units[Point(x,y)] = Unit(c, 200, 3)
            elif c == Grid.ELF:
                grid.units[Point(x,y)] = Unit(c, 200, elf_power)
    return grid

def get_play_order(grid: Grid) -> List[Point]:
    return sorted((p for p,c in grid.units.items()), key=sort_points)

def find_targets(grid: Grid, unit: Unit) -> Dict[Point, Unit]:
    return {p: u for p, u in grid.units.items() if u.type != unit.type}

def get_in_range(grid: Grid, attackerPos: Point, targets: Dict[Point,Unit]) -> Set[Point]:
    neighbors = flatten(grid.neighbors(p, allow=attackerPos) for p in targets.keys())
    return set(neighbors)

def flatten(l):
    return list(itertools.chain.from_iterable(l))

def choose_target(grid: Grid, pos: Point) -> Optional[Point]:
    (x,y) = pos
    attacker = grid.units[pos]
    neighbors = [Point(x, y - 1), Point(x - 1, y), Point(x + 1, y), Point(x, y + 1)]
    units = [(p,grid.units.get(p)) for p in neighbors]
    units = [(p,u) for p,u in units if u and u.type != attacker.type]
    if units:
        min_hp = min(u.hp for p,u in units)
        lowest_hp_locs = [p for p,u in units if u.hp == min_hp]
        return sorted(lowest_hp_locs, key=sort_points)[0]
    else:
        return None

def play_round(grid: Grid, elf_deaths_allowed: bool = True) -> bool:
    play_order = get_play_order(grid)
    all_done = False
    while play_order:
        pos = play_order.pop(0)
        unit = grid.units.get(pos)
        targets = find_targets(grid, unit)
        if not targets:
            all_done = True
            break
        if not try_attack(grid, pos, unit, play_order, elf_deaths_allowed):
            pos = move(grid, pos, unit, targets)
            try_attack(grid, pos, unit, play_order, elf_deaths_allowed)
    return all_done

# returns the new pos
def move(grid: Grid, pos: Point, unit: Unit, targets: Dict[Point, Unit]) -> Point:
    in_range = get_in_range(grid, pos, targets)
    dest = nearest_of(grid, pos, in_range)
    if dest:
        next_step = choose_next_step(grid, pos, dest)
        del grid.units[pos]
        grid.units[next_step] = unit
        # print(f"move {unit.type} at {pos} to {next_step}")
        return next_step
    return pos

# return true if something was attacked
def try_attack(grid, pos, unit, play_order, elf_deaths_allowed) -> bool:
    target_pos = choose_target(grid, pos)
    if target_pos:
        target_unit = grid.units[target_pos]
        updated_unit = Unit(target_unit.type, target_unit.hp-unit.attack, target_unit.attack)
        # print(f"{pos} attacking {target_unit.type} at {target_pos}, hp now {updated_unit.hp}")
        if updated_unit.hp <= 0:
            # print(f"{updated_unit.type} died at {target_pos}")
            if updated_unit.type == Grid.ELF and not elf_deaths_allowed:
                raise ElfDeath
            del grid.units[target_pos]
            try:
                play_order.remove(target_pos)
            except ValueError:
                pass
        else:
            grid.units[target_pos] = updated_unit
        return True
    else:
        return False

def get_total_hp(grid) -> int:
    return sum(u.hp for u in grid.units.values())

def ans1(grid: Grid, elf_deaths_allowed: bool = True) -> int:
    for i in itertools.count(1):
        # print(f"round {i}:")
        all_done = play_round(grid, elf_deaths_allowed)
        # print(grid)
        if all_done:
            break
    complete_rounds = i-1
    remaining_hp = get_total_hp(grid)
    # print(f"complete rounds: {complete_rounds}")
    # print(f"remaining hp: {remaining_hp}")
    return complete_rounds * remaining_hp

def ans2(input: str, starting_power: int) -> int:
    started_too_high = True
    for elf_power in itertools.count(starting_power):
        grid = parse(input, elf_power)
        try:
            outcome = ans1(grid, elf_deaths_allowed=False)
        except ElfDeath:
            started_too_high = False
            continue
        else:
            break
    if started_too_high:
        raise Exception("ans2 starting power was too high")
    else:
        return outcome

def main():
    with open("input.txt") as f:
        input = f.read()
    grid = parse(input)
    print(f"ans1 = {ans1(grid)}")
    print(f"ans2 = {ans2(input, 4)}")

if __name__ == "__main__":
    main()