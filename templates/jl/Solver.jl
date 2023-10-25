module Solver

include("../utils/jl/Utils.jl")
using .Utils

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


testinput = """
"""
testanswer_1 = 0
testanswer_2 = 0
export testinput, testanswer_1, testanswer_2

include("../utils/jl/AoC.jl")

end # module Solver
