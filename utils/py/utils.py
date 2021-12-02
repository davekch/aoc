from datetime import datetime
from functools import wraps


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

