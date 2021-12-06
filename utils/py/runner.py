from importlib import import_module
import sys
import os
import numpy as np
import matplotlib.pyplot as plt


def python_days(directory: str) -> list[str]:
    dirs = [d for d in os.listdir(directory) if os.path.isdir(os.path.join(directory, d))]
    return sorted(
        [d for d in dirs if "solver.py" in os.listdir(os.path.join(directory, d))]
    )


def run_solutions(days: list[str], directory: str, n: int = 5) -> dict[str, list[float]]:
    sys.path.insert(0, directory)
    sys.path.insert(0, os.path.join(directory, "utils/py"))
    times = {
        "parse": [],
        "solve1": [],
        "solve2": [],
        "total": [],
    }
    for day in days:
        with open(os.path.join(directory, day, "input.txt")) as f:
            raw_data = f.read().strip()

        solver = import_module(f"{day}.solver")
        parse_times = []
        solve1_times = []
        solve2_times = []
        totals = []
        for _ in range(n):
            data = solver.parse(raw_data)
            solver.solve1(data)
            solver.solve2(data)
            measured = dict(solver.measure_time.times)
            parse_times.append(measured["parse"])
            solve1_times.append(measured["solve1"])
            solve2_times.append(measured["solve2"])
            totals.append(sum(measured.values()))

        times["parse"].append(np.mean(parse_times))
        times["solve1"].append(np.mean(solve1_times))
        times["solve2"].append(np.mean(solve2_times))
        times["total"].append(np.mean(totals))
    return times


def plot_times(days: list[str], times: dict[str, list[float]]):
    plt.plot(days, times["parse"], label="parse")
    plt.plot(days, times["solve1"], label="solve part 1")
    plt.plot(days, times["solve2"], label="solve part 2")
    plt.plot(days, times["total"], label="total")
    plt.yscale("log")
    plt.ylabel("runtime in s")
    plt.legend()
    plt.show()


if __name__ == "__main__":
    directory = "../.."
    days = python_days(directory)
    times = run_solutions(days, directory)
    plot_times(days, times)
