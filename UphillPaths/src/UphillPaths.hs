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
gen n = gen' n (P 1 1) $ S.fromList [P 1 1, P n n]
  where
    gen' :: Int -> Point -> S.Set Point -> S.Set Point
    gen' n p s | next n p `S.member` s = s
               | otherwise = gen' n (next n p) (S.insert (next n p) s)
    next !n (P !x !y) = P (x * 2 `mod` n) (y * 3 `mod` n)

-- solving using longest increasing subsequence problem on the y coordinates 

s :: Int -> Int
s n = lis (gen n) - 2
 -- todo: understand and optimize.
lis :: S.Set Point -> Int
lis xs = runST $ do
  pileTops <- newSTUArray (1, S.size xs)
  foldM (stack pileTops) 1 xs
 
stack piles i p@(P _ y) = do
  max <- readArray piles i
  if max <= y then writeArray piles (i+1) y
    else do
      j <- bsearch piles y i
      writeArray piles j y
  return $ if max <= y then i+1 else i

bsearch piles x = go 1
  where go lo hi | lo > hi   = return lo
                 | otherwise =
                    do y <- readArray piles mid
                       if y <= x then go (succ mid) hi
                                else go lo (pred mid)
 
                         where mid = (lo + hi) `div` 2
 
newSTUArray :: Ix l => (l,l) -> ST o (STUArray o l Int)
newSTUArray = newArray_