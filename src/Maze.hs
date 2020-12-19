{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
module Maze where

import Control.Monad
import Control.Monad.State.Class
import Control.Monad.State (evalStateT)
import Control.Monad.Writer.Class (MonadWriter, tell, censor)
import Control.Monad.Writer (execWriter)
import Data.Array (Array, Ix, (!), (//))
import qualified Data.Array as A
import System.Random (RandomGen, randomR, newStdGen)
import Debug.Trace

data Dir = N | W deriving (Eq, Ord, Read, Show, Ix)
data Pos = Pos Int Int Dir deriving (Eq, Ord, Read, Show, Ix)
data Edge = Passage | Wall deriving (Eq, Ord, Read, Show)
type EdgeGrid = Array Pos Edge
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
    where (Pos h1 w1 _, Pos h2 w2 _) = A.bounds g
          go i = (drawHorizontal (proj N), drawVertical (proj W))
            where proj d = [ g ! Pos i j d | j <- [w1..w2]]

ppAC :: Int -> AlternatingCycle a -> [a]
ppAC n (AC xs@((c, _):_)) = foldr (\(a, b) acc -> a : replicate n b ++ acc) [c] xs

ppAC2 :: Int -> AlternatingCycle (AlternatingCycle a) -> [[a]]
ppAC2 n ac = ppAC n <$> ppAC n ac

ppMaze :: Maze -> String
ppMaze = unlines . ppAC2 2 . drawMaze

putMaze :: Maze -> IO ()
putMaze = putStr . ppMaze

allWalls :: (Int, Int) -> EdgeGrid
allWalls (i, j) = fmap (const Wall) $ A.array (Pos 0 0 N, Pos (i-1) (j-1) W) []

-- When used in the context of a split, the direction indicates the kind of wall being affected;
-- i.e., N means that the split is horizontal, W means the split is vertical.
shiftV, shiftH :: Int -> Pos -> Pos
shiftV n (Pos i j d) = Pos (i+n) j d
shiftH n (Pos i j d) = Pos i (j+n) d
shiftD :: Dir -> Int -> Pos -> Pos
shiftD N k (Pos i j d) = Pos (i+k) j d
shiftD W k (Pos i j d) = Pos i (j+k) d

getLeft, getRight :: Dir -> Int -> (Int, Int) -> (Int, Int)
getLeft N k (n, m) = (k, m)
getLeft W k (n, m) = (n, k)
getRight N k (n, m) = (n-k, m)
getRight W k (n, m) = (n, m-k)

genDoor :: Dir -> Int -> Int -> Pos
genDoor N k l = Pos k l N
genDoor W k l = Pos l k W

gen :: (RandomGen g, MonadState g m) => Int -> m Int
gen n = state $ randomR (0, n-1)

decideSplit :: (RandomGen g, MonadState g m) => (Int, Int) -> m (Int, Int, Dir)
decideSplit (n, m) = do
    k <- gen $ n+m-2
    if k <= n-2
    then do
        l <- gen $ m-1
        return (k+1, l+1, N)
    else do
        l <- gen $ n-1
        return (k-n+2, l+1, W)

unify :: (RandomGen g, MonadState g m, MonadWriter [Pos] m) => (Int, Int) -> m ()
unify (1, n) = tell $ map (\i -> Pos 0 i W) [1..n-1]
unify (n, 1) = tell $ map (\i -> Pos i 0 N) [1..n-1]
unify p = do
    (k, l, d) <- decideSplit p
    tell [genDoor d k l]
    unify $ getLeft d k p
    censor (map $ shiftD d k) $ unify $ getRight d k p

generate :: Int -> IO Maze
generate n = go <$> newStdGen
    where p = (n, n)
          go g = Maze $ allWalls p // (map (\i -> (i, Passage)) $ execWriter $ evalStateT (unify p) g)
