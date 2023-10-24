using Test


# the functions here intentionally don't take arguments
# this file is to be included at the end of each solver.jl module
# to allow quick `main()` and `test()` calls in the REPL
# parse_input, solve1, solve2, testinput, testanswer_{1,2} must
# be defined in solver.jl


function main()
    raw_data = read("input.txt", String)
    t_p = @elapsed parsed = parse_input(raw_data)
    t_1 = @elapsed part1 = solve1(parsed)
    t_2 = @elapsed part2 = solve2(parsed)

    println("part 1: $part1")
    println("part 2: $part2")

    println("------------------")
    println(" parse time: $t_p")
    println("part 1 time: $t_1")
    println("part 2 time: $t_2")
    println("        sum: $(t_p + t_1 + t_2)")
end
export main


function test()
    testparsed = parse_input(testinput)
    @test solve1(testparsed) == testanswer_1
    @test solve2(testparsed) == testanswer_2
end
export test
