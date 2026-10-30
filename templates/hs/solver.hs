{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ
import Data.List
import AoC
import AoC.Utils
import AoC.CLI

type Parsed = [Int]
type Sol1 = Int
type Sol2 = Int

parse :: String -> Parsed
parse input = undefined

solve1 :: Parsed -> Sol1
solve1 parsed = undefined

solve2 :: Parsed -> Sol2
solve2 parsed = undefined


testdata = [r||]
testresult1 = 0
testresult2 = 0

main = main' parse solve1 solve2 testdata testresult1 testresult2
