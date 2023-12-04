module Solver
using Test
using AoC
using AoC.Utils


function parse_input(raw_data)
    raw_data
end
export parse_input


function solve1(parsed)
end
export solve1


function solve2(parsed)
end
export solve2


solution = Solution(parse_input, solve1, solve2)

testinput = """
"""
testanswer_1 = nothing
testanswer_2 = nothing
export testinput, testanswer_1, testanswer_2

test() = AoC.test_solution(solution, testinput, testanswer_1, testanswer_2)
export test

main(part=missing) = AoC.main(solution, part)
export main


end # module Solver
