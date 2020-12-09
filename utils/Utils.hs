module Utils
( parseInts
, test
, groupByEmptyLines
, Command
, BootStatus (INFLOOP, DONE)
, strToCommand
, bootLoader
) where

import Data.Function (on)
import Data.Char
import Data.List
import Data.List.Split (splitOn)
import Text.Read


-- parse every int in a string to a list of ints, ignoring non-digit characters
parseInts :: String  -> [Int]
parseInts = foldr getInt [] . groupBy ((==) `on` isDigit)
    where
        getInt str ls = case (readMaybe str) of 
            Nothing -> ls
            Just n -> n:ls


-- splits a string by lines but parts which are seperated by an empty
-- line are grouped together
groupByEmptyLines :: String -> [[String]]
groupByEmptyLines = map lines . splitOn "\n\n"


data TestResult a = Ok | Fail a a

instance (Show a) => Show (TestResult a) where
    show Ok = "Test passed."
    show (Fail a b) = "Test failed! Expected " ++ show a ++ " but got " ++ show b

test :: (Eq b) => (a -> b) -> b -> a -> TestResult b
test f expected x
    | out == expected = Ok
    | otherwise = Fail expected out
    where
        out = f x

        
        
-- ========================= bootloader ======================
data Instruction = NOP | ACC | JMP
data Command = Command Instruction Int
data BootStatus = INFLOOP | DONE deriving (Eq)

strToCommand :: String -> Command
strToCommand command = case com of
    "nop" -> Command NOP value
    "acc" -> Command ACC value
    "jmp" -> Command JMP value
    where
        (com:(s:v):_) = words command
        f = if s == '-' then negate else id  -- check the sign of the integer
        value = f $ read v

bootLoader' :: Int -> Int -> [Int] -> [Command] -> (BootStatus, Int)
bootLoader' acc pos history code
    | pos `elem` history = (INFLOOP, acc)
    | pos >= length code    = (DONE, acc)
    | otherwise = case code !! pos of
        (Command NOP _)     -> bootLoader' acc         (pos+1) (pos:history) code
        (Command ACC value) -> bootLoader' (acc+value) (pos+1) (pos:history) code
        (Command JMP value) -> bootLoader' acc     (pos+value) (pos:history) code

bootLoader :: [Command] -> (BootStatus, Int)
bootLoader = bootLoader' 0 0 []
