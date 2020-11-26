import Data.List
import Utils

getInput :: IO String
getInput = do
    contents <- readFile "input.txt"
    return contents


parse :: String -> a
parse input = undefined


solve1 :: a -> String
solve1 parsed = undefined


solve2 :: a -> String
solve2 parsed = undefined


testdata = ""
testresult1 = ""
testresult2 = ""
test1 = test (solve1 . parse) testdata testresult1
test2 = test (solve2 . parse) testdata testresult2


main = do 
    raw_input <- getInput
    let parsed = parse raw_input
    let sol1 = solve1 parsed 
    putStrLn ("Part 1: " ++ sol1)
    let sol2 = solve2 parsed 
    putStrLn ("Part 2: " ++ sol2)
