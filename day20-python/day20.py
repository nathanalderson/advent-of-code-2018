from parsimonious import *
import logging
import sys
from collections import namedtuple, defaultdict, deque
from typing import *
import itertools

sys.setrecursionlimit(2500)
logging.basicConfig(level=logging.INFO)
log = logging.getLogger(__name__)

Point = namedtuple('Point', 'x, y')

Graph = Dict[Point, Set[Point]]

class Node():
    def __init__(self):
        self.type = "Generic"
        self.val = None

    def __str__(self):
        return "{}: {}".format(self.type, self.val)

    def __repr__(self):
        return str(self)

    def __eq__(self, other):
        return self.type == other.type and self.val == other.val

class Sequence(Node):
    def __init__(self, val):
        super().__init__()
        self.type = "Seq"
        self.val = val

class Branch(Node):
    def __init__(self, *args):
        super().__init__()
        self.type = "Branch"
        self.val = args

grammar = Grammar(
    r"""
    regex = "^" tokens "$"
    tokens = token*
    token = (sequence / branch)
    sequence = ~"[NSEW]+"
    branch = "(" tokens "|" tokens ("|" tokens)* ")"
    """)

class RegexVisitor(NodeVisitor):
    def __init__(self):
        pass

    def visit_regex(self, node, regex):
        _, tokens, _ = regex
        log.debug("command: %s", tokens)
        return tokens

    def visit_tokens(self, node, tokens):
        log.debug("tokens: %s", tokens)
        return tokens

    def visit_token(self, node, token):
        t = token
        t = t[0]
        log.debug("token: %s", t)
        return t

    def visit_sequence(self, node, sequence):
        i = Sequence(node.text)
        log.debug("Sequence: %s", i)
        return i

    def visit_branch(self, node, branch):
        _, t1, _, t2, rest, _ = branch
        rest = rest or []
        rest = (t for _, t in rest)
        a = Branch(t1, t2, *rest)
        log.debug("branch: %s", a)
        return a

    def generic_visit(self, node, children):
        log.debug("generic: %s", node.text)
        return children or None

def parse(data):
    try:
        parsed = grammar.parse(data)
    except ParseError as e:
        raise ValueError(str(e)) from e
    log.debug(parsed)
    visitor = RegexVisitor()
    result = visitor.visit(parsed)
    log.debug(result)
    return result

def follow(path: List, positions: Set[Point] = None, graph: Graph = None) -> (Graph, Set[Point]):
    log.debug("follow:", path, "positions:", positions)
    positions = positions or {Point(0,0)}
    graph = graph or defaultdict(set)
    for node in path:
        if node.type == "Seq":
            positions = {followSeq(node, pos, graph) for pos in positions}
        elif node.type == "Branch":
            positions = flatten(follow(path, positions, graph)[1] for path in node.val)
    return graph, positions

def followSeq(seq: Sequence, pos: Point, graph: Graph) -> Point:
    log.debug("followSeq:", seq, "pos:", pos)
    for dir in seq.val:
        newPos = go(dir, pos)
        addReachable(pos, newPos, graph)
        pos = newPos
    return pos

def go(dir: str, pos: Point) -> Point:
    if   dir == "N": return Point(pos.x, pos.y+1)
    elif dir == "S": return Point(pos.x, pos.y-1)
    elif dir == "E": return Point(pos.x+1, pos.y)
    elif dir == "W": return Point(pos.x-1, pos.y)
    else: raise ValueError("Unknown direction:", dir)

def addReachable(p1: Point, p2: Point, graph: Graph) -> None:
    graph[p1].add(p2)
    graph[p2].add(p1)

def flatten(l):
    try:
        return list(itertools.chain.from_iterable(l))
    except TypeError:
        return l

def get_dists(graph: Graph, start) -> Dict[Point, int]:
    frontier = deque([start])
    dists = {start: 0}
    while len(frontier) > 0:
        current = frontier.popleft()
        for next in graph[current]:
            if next not in dists:
                frontier.append(next)
                dists[next] = dists[current] + 1
    return dists

def main():
    with open("input.txt") as f:
        data = f.read().strip()
    parsed = parse(data)
    graph = follow(parsed)[0]
    dists = get_dists(graph, Point(0,0))
    print("ans1 =", dists[max(dists, key=dists.get)])
    print("ans2 =", len([p for p,d in dists.items() if d >= 1000]))

if __name__ == "__main__":
    main()