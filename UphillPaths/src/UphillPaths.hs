{-# LANGUAGE FlexibleContexts, BangPatterns #-}
module UphillPaths where

import qualified Data.Set as S

import Control.Monad.ST  ( ST, runST )
import Control.Monad
import Data.Array.ST
import Data.Array.MArray
import Point

import Debug.Trace

traceMonad :: (Show a, Monad m) => a -> m a
traceMonad x = trace ("test: " ++ show x) (return x)

gen :: Int -> S.Set Point
gen !n = gen' n (P 1 1) $ S.fromList [P 1 1, P n n]
  where
    gen' :: Int -> Point -> S.Set Point -> S.Set Point
    gen' !n !p !s | () !n !p !s !False = undefined --unbox
                  | next n p `S.member` s = s
                  | otherwise = gen' n (next n p) (S.insert (next n p) s)
        where
          next !n (P !x !y) = P (x * 2 `mod` n) (y * 3 `mod` n)

-- solving using longest increasing subsequence problem on the y coordinates 

s :: Int -> Int
s n = lis (gen n) - 2

lis :: S.Set Point -> Int
lis xs = fst $ runST $ do
  pileTops <- newSTUArray (1, max 5 $ S.size xs)
  foldM (stack pileTops) (1, 0) xs
 
stack piles (!i, !max) (P _ y) 
  | max <= y = do
    writeArray piles (i+1) y
    return (i + 1, y)
  | otherwise = do
    j <- bsearch piles y i
    writeArray piles j y
    return (i, max)

bsearch piles x = go 1
  where go lo hi | lo > hi   = return lo
                 | otherwise =
                    do y <- readArray piles mid
                       if y <= x then go (succ mid) hi
                                else go lo (pred mid)
 
                         where mid = (lo + hi) `div` 2
 
newSTUArray :: (Int, Int) -> ST o (STUArray o Int Int)
newSTUArray = newArray_