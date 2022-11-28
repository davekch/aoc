module Utils
( parseInts
, test
, groupByEmptyLines
, diff
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
