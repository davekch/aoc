module Geometry where

import qualified Data.Vector as Vec
import Data.Vector (Vector)
import qualified Data.Map as Map
import Data.Map (Map)


type Grid a b = Map (Vector a) b


mkVec :: [a] -> Vector a
mkVec = Vec.fromList

vadd :: Num a => Vector a -> Vector a -> Vector a
vadd = Vec.zipWith (+)

vsub :: Num a => Vector a -> Vector a -> Vector a
vsub = Vec.zipWith (-)

dot :: Num a => Vector a -> Vector a -> a
dot x y = Vec.sum $ Vec.zipWith (*) x y

-- multiply matrix on vector; should work in N dimensions
{-
    note on implementation:
    Vec.map dot mat :: Vector (Vector a -> a)
    so a Vector of functions that takes a Vector and returns a number.
    this vector of functions is the dot product partially applied with all
    rows of the matrix
    so we want to apply all of this to the input vector which can be
    achieved with <*> mkVek [v]
-}
matmul :: Num a => Vector (Vector a) -> Vector a -> Vector a
matmul mat v = (Vec.map dot mat) <*> (mkVec [v])

-- calculates the manhattan distance between x and y in N dimensions
manhattan :: (Num a) => Vector a -> Vector a -> a
manhattan x y = Vec.sum . Vec.map abs $ vsub x y

-- rotation matrix in 2D 90 degrees clockwise
rot2D90 = mkVec [
        mkVec [0,  1],
        mkVec [-1, 0]
    ]

------------------------------------------------------------ Grid stuff --------------------------

-- go through the grid according to a function that computes a new point
-- until it reaches a point that is not in the grid
-- return list of grid points where we walked along
walkGrid :: Ord a => Grid a b -> (Vector a -> Vector a) -> Vector a -> [(Vector a, b)]
walkGrid grid f start = case Map.lookup destination grid of
    Nothing -> []
    Just val -> (destination, val) : walkGrid grid f destination
    where destination = f start

-- takes a string and returns a 2D grid with each char at its
-- x-y position in the string
gridFromString :: String -> Grid Int Char
gridFromString input = Map.fromList (index enumerated)
    where
        enumerated = zip [0..] . map (zip [0..]) . lines $ input
        index = concat . map indexLine
        indexLine ((_, [])) = []
        indexLine ((y, (x, v):vs)) = (mkVec [x,y], v) : indexLine (y, vs)
