module Solver
using Test
using AoC
using AoC.Utils


function parse_input(raw_data)
    raw_data
end


function solve1(parsed)
end


function solve2(parsed)
end


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
