import Data.List
import Utils

getInput :: IO String
getInput = do
    contents <- readFile "input.txt"
    return contents


type Parsed = [Int]
type Sol1 = Int
type Sol2 = Int



parse :: String -> Parsed
parse input = undefined


solve1 :: Parsed -> Sol1
solve1 parsed = undefined


solve2 :: Parsed -> Sol2
solve2 parsed = undefined


testdata = ""
testresult1 = 0
testresult2 = 0
test1 = test (solve1 . parse) testresult1 testdata
test2 = test (solve2 . parse) testresult2 testdata


main = do
    raw_input <- getInput
    let parsed = parse raw_input
    let sol1 = solve1 parsed
    putStrLn ("Part 1: " ++ show sol1)
    let sol2 = solve2 parsed
    putStrLn ("Part 2: " ++ show sol2)
