{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Utils where

import Data.Function (on)
import Data.Char
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Set as Set
import Data.Set (Set)
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


enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive f (x:xs) = x : if f x then takeWhileInclusive f xs else []

-------------------------------------------------- Tree stuff -----------

data Tree a = Node a | Tree a [Tree a] deriving (Show, Eq, Foldable)

instance Functor Tree where
    fmap f (Tree x children) = Tree (f x) (map (fmap f) children)
    fmap f (Node x) = Node (f x)

getValue :: Tree a -> a
getValue (Node x) = x
getValue (Tree x _) = x

-- try do to down the tree according to the values in the list
-- eg walk [1,2,3] Tree 1 [Tree 2 [Node 3], Node 4, Tree 5 []]
-- would return Just Node 3
walk :: Eq a => [a] -> Tree a -> Maybe (Tree a)
walk = walkBy (==)

-- see walk
walkBy :: (a -> b -> Bool) -> [b] -> Tree a -> Maybe (Tree a)
walkBy f (x:[]) tree = if f (getValue tree) x then Just tree else Nothing
walkBy f (x1:x2:xs) tree = case tree of
    Node _ -> Nothing  -- we can't walk into a node
    Tree y children -> if f y x1
        then
            case find ((flip f x2) . getValue) children of
                Just subtree -> walkBy f (x2:xs) subtree
                Nothing -> Nothing
        else
            Nothing

isNode :: Tree a -> Bool
isNode (Node _) = True
isNode _ = False

prettyShow :: Show a => Tree a -> String
prettyShow tree = prettyShow' 0 tree ++ "\n"
    where
        prettyShow' depth node@(Node _) = replicate depth ' ' ++ show node
        prettyShow' depth (Tree x children) = replicate depth ' ' ++ "Tree " ++ show x ++ " [\n" ++ (intercalate "\n" $ map (prettyShow' (depth + 4)) children) ++ "\n" ++ replicate depth ' ' ++ "]"

------------------------------------------- end of tree stuff -------------

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
