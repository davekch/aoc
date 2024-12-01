#!/usr/bin/env python

from aoc import utils

watch = utils.stopwatch()


@watch.measure_time
def parse(raw_data):
    pass


@watch.measure_time
def solve1(data):
    pass


@watch.measure_time
def solve2(data):
    pass


if __name__ == "__main__":
    data = parse(open("input.txt").read().strip())
    print(f"Part 1: {solve1(data)}")
    print(f"Part 2: {solve2(data)}")
    print()
    watch.print_times()

