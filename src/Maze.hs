{-# LANGUAGE DeriveFunctor #-}
module Maze where

import qualified Data.Array as A
import Data.Array (Array, Ix, (!))

data Dir = N | W deriving (Eq, Ord, Read, Show, Ix)
data Edge = Passage | Wall deriving (Eq, Ord, Read, Show)
type EdgeGrid = Array (Int, Int, Dir) Edge
newtype Maze = Maze EdgeGrid deriving (Eq, Ord, Read, Show)

-- This describes an iterating pattern abababa, where the last a is the same as the first a and the
-- list stores pairs (ab).
newtype AlternatingCycle a = AC [(a, a)] deriving (Eq, Ord, Read, Show, Functor)

drawVertical :: [Edge] -> AlternatingCycle Char
drawVertical = AC . map (\x -> (ppEdge x, ' '))
    where ppEdge Passage = ' '
          ppEdge Wall = '|'

drawHorizontal :: [Edge] -> AlternatingCycle Char
drawHorizontal = AC . map (\x -> ('+', ppEdge x))
    where ppEdge Passage = ' '
          ppEdge Wall = '-'

drawMaze :: Maze -> AlternatingCycle (AlternatingCycle Char)
drawMaze (Maze g) = AC $ map go [h1..h2]
    where ((h1, w1, _), (h2, w2, _)) = A.bounds g
          go i = (drawHorizontal (proj N), drawVertical (proj W))
            where proj d = [ g ! (i, j, d) | j <- [w1..w2]]

ppAC :: Int -> AlternatingCycle a -> [a]
ppAC n (AC xs@((c, _):_)) = foldr (\(a, b) acc -> a : replicate n b ++ acc) [c] xs

ppAC2 :: Int -> AlternatingCycle (AlternatingCycle a) -> [[a]]
ppAC2 n ac = ppAC n <$> ppAC n ac


