from aoc import utils

measure_time = utils.stopwatch()


@measure_time
def parse(raw_data):
    pass


# PART 1
@measure_time
def solve1(data):
    pass


# PART 2
@measure_time
def solve2(data):
    pass


if __name__ == "__main__":
    data = parse(open("input.txt").read().strip())
    print(f"Part 1: {solve1(data)}")
    print(f"Part 2: {solve2(data)}")

    print("\nTime taken:")
    for func, time in measure_time.times:
        print(f"{func:8}{time}s")
    print("----------------")
    print(f"total   {sum(t for _, t in measure_time.times)}s")
