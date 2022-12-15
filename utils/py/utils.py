from datetime import datetime
from functools import wraps
from typing import Dict, Any

from rich.traceback import install
install(show_locals=False)


def corners(grid: Dict[tuple[int], Any]):
    """takes a dict that maps points to something and returns
    minx, maxx, miny, maxy of the outermost points
    """
    xs = [x for x,_ in grid.keys()]
    ys = [y for _,y in grid.keys()]
    return min(xs), max(xs), min(ys), max(ys)


def dictgrid_to_str(grid: dict[tuple[int]], empty=" ") -> str:
    """converts a dict that maps 2D points to values to a printable
    grid string. positive y-axis points down"""
    minx, maxx, miny, maxy = corners(grid)
    img = ""
    for y in range(miny, maxy+1):
        for x in range(minx, maxx+1):
            if (x,y) in grid:
                p = grid[(x,y)]
            else:
                p = empty
            img += str(p)
        img += "\n"
    return img


def stopwatch():
    """a factory for time measurement decorators"""
    times = []
    def measure_time(func):
        @wraps(func)
        def _func(*args, **kwargs):
            start = datetime.now()
            result = func(*args, **kwargs)
            end = datetime.now()
            times.append((func.__name__, (end - start).total_seconds()))
            return result

        return _func
    measure_time.times = times
    return measure_time

