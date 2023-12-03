module AoC
using Test

include("Utils.jl")


struct Solution
    parse_input::Function
    part1::Function
    part2::Union{Function, Nothing}  # for when one function solves both parts
end
function Solution(parse_input::Function, solve::Function)
    Solution(parse_input, solve, nothing)
end
export Solution


function main(solution::Solution, part=missing)
    raw_data = read("input.txt", String)
    t_p = @elapsed parsed = solution.parse_input(raw_data)
    if part === missing || part == 1
        t_1 = @elapsed part1 = solution.part1(parsed)
        if solution.part2 === nothing
            (part1, part2) = part1
            println("part 1: $part1")
            println("part 2: $part2")
        else
            println("part 1: $part1")
        end
    else
        t_1 = 0
    end
    if (part === missing || part == 2) && solution.part2 !== nothing
        t_2 = @elapsed part2 = solution.part2(parsed)
        println("part 2: $part2")
    else
        t_2 = 0
    end

    println("------------------")
    println(" parse time: $t_p")
    println("part 1 time: $t_1")
    println("part 2 time: $t_2")
    println("        sum: $(t_p + t_1 + t_2)")
end


function test_solution(solution::Solution, testinput, testanswer_1, testanswer_2)
    testparsed = solution.parse_input(testinput)
    if solution.part2 !== nothing
        part1 = solution.part1(testparsed)
        part2 = solution.part2(testparsed)
    else
        (part1, part2) = solution.part1(testparsed)
    end
    @info @test part1 == testanswer_1
    @info @test part2 == testanswer_2
end
export test_solution


end  # module AoC
