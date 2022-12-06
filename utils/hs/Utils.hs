module Utils
( parseInts
, test
, groupByEmptyLines
, diff
, intersections
, takeEvery
, linesWith
, unique
, slide
, slideWith
, Point2D (Point2D)
, turnL
, turnR
, manhattan
, clioptions
, CLIOptions (CLIOptions)
) where

import Data.Function (on)
import Data.Char
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Set as Set
import Data.Set (Set)
import Options.Applicative
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


-- get common elements from a list of sets
intersections :: Ord a => [Set a] -> Set a
intersections ss = foldl Set.intersection (head ss) (tail ss)


-- take every nth element from a list
takeEvery :: Int -> [a] -> [a]
takeEvery n [] = []
takeEvery n (x:ls) = x : takeEvery n (drop (n-1) ls)


linesWith :: (String -> a) -> String -> [a]
linesWith f = map f . lines


-- check whether all elements in a list are unique
unique :: Ord a => [a] -> Bool
unique ls = length ls == Set.size (Set.fromList ls)

-- sliding window with window size n
slide :: Int -> [a] -> [[a]]
slide n [] = []
slide n ls = take n ls : slide n (tail ls)

-- sliding window but also apply f
slideWith :: ([a] -> b) -> Int -> [a] -> [b]
slideWith f n = map f . slide n

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


diff :: (Num a) => [a] -> [a]
diff []     = []
diff (_:[]) = []
diff (x1:x2:xs) = (x2-x1) : diff (x2:xs)


-- ==================================================== 2D geometry
data Point2D a = Point2D { x :: a, y :: a } deriving (Show, Eq)

-- rotate a Point2D to the right by 90 degrees n times
turnR :: (Num a) => Point2D a -> Int -> Point2D a
turnR dir 0 = dir
turnR (Point2D x y) 1 = Point2D y (-x) 
turnR dir turns = turnR (turnR dir 1) (turns-1)

-- rotate a Point2D to the left by 90 degrees n times
turnL :: (Num a) => Point2D a -> Int -> Point2D a
turnL dir 0 = dir
turnL (Point2D x y) 1 = Point2D (-y) x 
turnL dir turns = turnL (turnL dir 1) (turns-1)

manhattan :: (Num a) => Point2D a -> a
manhattan (Point2D x y) = abs x + abs y

-- ============================================== CLI option parser

data CLIOptions = CLIOptions
    { clipart :: Int
    , clitest :: Bool
    }
    deriving (Show)

cliparser :: Parser CLIOptions
cliparser =
    CLIOptions
        <$> option auto
            ( long "part"
            <> short 'p'
            <> metavar "PART"
            <> help "specify a part"
            <> value 0
            )
        <*> switch
            ( long "test"
            <> short 't'
            <> help "run tests"
            )


clioptions :: IO CLIOptions
clioptions = execParser options
    where
        options = info (cliparser <**> helper) fullDesc
