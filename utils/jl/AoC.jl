using Test


# the functions here intentionally don't take arguments
# this file is to be included at the end of each solver.jl module
# to allow quick `main()` and `test()` calls in the REPL
# parse_input, solve1, solve2, testinput, testanswer_{1,2} must
# be defined in solver.jl


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


function test(part=missing)
    testparsed = parse_input(testinput)
    if part === 1 || part === missing
        @info @test solve1(testparsed) == testanswer_1
    end
    if part === 2 || part === missing
        @info @test solve2(testparsed) == testanswer_2
    end
end
export test
