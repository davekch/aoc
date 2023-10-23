using Test

function parse_input(raw_data)
    raw_data
end

function solve1(parsed)
end

function solve2(parsed)
end


# ------------------ testing ------------------ 
testinput = """
"""

function test()
    testparsed = parse_input(testinput)
    @test solve1(testparsed) == 0
    @test solve2(testparsed) == 0
end

# --------------------- main ------------------
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


test()
main()
