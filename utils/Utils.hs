module Utils
( parseInts
) where

import Data.Function (on)
import Data.Char
import Data.List
import Text.Read


-- parse every int in a string to a list of ints, ignoring non-digit characters
parseInts :: String  -> [Int]
parseInts = foldr getInt [] . groupBy ((==) `on` isDigit)
    where
        getInt str ls = case (readMaybe str) of 
            Nothing -> ls
            Just n -> n:ls
