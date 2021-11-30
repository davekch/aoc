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
    parsed <- parse <$> getInput
    putStr "Part 1: "
    print . solve1 $ parsed
    putStr "Part 2: "
    print . solve2 $ parsed
