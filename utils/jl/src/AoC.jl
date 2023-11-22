module AoC
using Test

include("Utils.jl")


abstract type AoCInput end
abstract type AoCParsed end

# define methods for these functions in each solver
parse_input(i::AoCInput) = missing
export parse_input
solve1(p::AoCParsed) = missing
export solve1
solve2(p::AoCParsed) = missing
export solve2


function main(part=missing)
    raw_data = read("input.txt", String)
    t_p = @elapsed parsed = parse_input(raw_data)
    if part === 1 || part === missing
        t_1 = @elapsed part1 = solve1(parsed)
        println("part 1: $part1")
    else
        t_1 = 0
    end
    if part === 2 || part === missing
        t_2 = @elapsed part2 = solve2(parsed)
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
export main


function test_solution(testinput, testanswer_1, testanswer_2, part=missing)
    testparsed = parse_input(testinput)
    if part === 1 || part === missing
        @info @test solve1(testparsed) == testanswer_1
    end
    if part === 2 || part === missing
        @info @test solve2(testparsed) == testanswer_2
    end
end
export test_solution


end  # module AoC