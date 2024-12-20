from datetime import datetime
from functools import wraps
from typing import TypeVar, Mapping
import re


def ints(s: str) -> list[int]:
    return list(map(int, re.findall(r"[+-]?\d+", s)))


def corners(points):
    """takes an iterable of points and returns
    minx, maxx, miny, maxy of the outermost points
    """
    xs = [x for x,_ in points]
    ys = [y for _,y in points]
    return min(xs), max(xs), min(ys), max(ys)


def _mktuple(*args):
    return tuple(args)


def dictgrid_to_str(grid: dict[tuple[int]], empty=" ", keybuilder=_mktuple) -> str:
    """converts a dict that maps 2D points to values to a printable
    grid string. positive y-axis points down"""
    minx, maxx, miny, maxy = corners(grid)
    img = ""
    for y in range(miny, maxy+1):
        for x in range(minx, maxx+1):
            if keybuilder(x,y) in grid:
                p = grid[keybuilder(x,y)]
            else:
                p = empty
            img += str(p)
        img += "\n"
    return img


def str_to_grid_dict(input: str, keybuilder=_mktuple) -> dict:
    """
    read a string into a (x,y)->chr dict
    """
    grid = {}
    for y, line in enumerate(input.splitlines()):
        for x, c in enumerate(line):
            grid[keybuilder(x, y)] = c
    return grid


K = TypeVar("K")
V = TypeVar("V")

def key_of_value(d: Mapping[K, V], v: V) -> K | None:
    """
    get the first key in d that has value v or None
    """
    for k, vv in d.items():
        if v == vv:
            return k


class stopwatch:
    def __init__(self):
        self.times = []

    def measure_time(self, f):
        """
        decorator to measure a function's runtime
        """
        @wraps(f)
        def _f(*args, **kwargs):
            start = datetime.now()
            result = f(*args, **kwargs)
            end = datetime.now()
            self.times.append((f.__name__, (end - start).total_seconds()))
            return result

        return _f

    def print_times(self):
        print("Time taken:")
        for func, time in self.times:
            print(f"{func:8}{time}s")
        print("----------------")
        print(f"total   {sum(t for _, t in self.times)}s")

