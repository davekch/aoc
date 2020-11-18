import pytest
from solver import parse, solve1, solve2

TESTDATA = """
"""

data = None


def test_parse():
    global data
    data = parse(TESTDATA)
    # asserts go here


# PART 1
def test_solve1():
    solution = solve1(data)
    # asserts go here


# PART 2
def test_solve2():
    solution = solve2(data)
    # asserts go here
