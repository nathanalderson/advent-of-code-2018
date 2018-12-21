from parsimonious import *
import logging
import sys

sys.setrecursionlimit(2500)
logging.basicConfig(level=logging.INFO)
log = logging.getLogger(__name__)

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

    def __str__(self):
        return self.val

class Branch(Node):
    def __init__(self, *args):
        super().__init__()
        self.type = "Branch"
        self.val = args

    def __str__(self):
        return str(self.val)


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

def main():
    with open("input.txt") as f:
        data = f.read().strip()
    print(parse(data))

if __name__ == "__main__":
    main()