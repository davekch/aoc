from . import utils
from argparse import ArgumentParser
from types import GeneratorType


def get_args():
    parser = ArgumentParser()
    parser.add_argument("-p", "--part", type=int)
    parser.add_argument("-i", "--input", default="input.txt")
    return parser.parse_args()


def main(parse, solve1, solve2=None):
    args = get_args()

    watch = utils.stopwatch()
    data = watch.measure_time(parse)(
        open(args.input).read().strip()
    )
    if not solve2:
        # solve1 solves both parts
        solution = watch.measure_time(solve1)(data)
        # we accept either both results at the same time or a generator
        if isinstance(solution, GeneratorType):
            solve1 = solve2 = next
            data = solution
        else:
            solve1 = lambda *_: solution[0]
            solve2 = lambda *_: solution[1]

    if not args.part or args.part == 1:
        sol1 = watch.measure_time(solve1)(data)
        print(f"Part 1: {sol1}")
    if not args.part or args.part == 2:
        sol2 = watch.measure_time(solve2)(data)
        print(f"Part 2: {sol2}")
    print()
    watch.print_times()

