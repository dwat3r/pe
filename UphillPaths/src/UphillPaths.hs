{-# LANGUAGE FlexibleContexts #-}
module UphillPaths where

import qualified Data.Set as Set

import Control.Monad.ST  ( ST, runST )
import Control.Monad     ( (>>=), (=<<), foldM )
import Data.Array.ST     ( Ix,  STArray, readArray, writeArray, newArray )
import Data.Array.MArray ( MArray )

gen :: Int -> [(Int,Int)]
gen n = Set.toList $ gen' n (1, 1) $ Set.fromList [(1,1),(n, n)]
  where
    gen' :: Int -> (Int,Int) -> Set.Set (Int,Int) -> Set.Set (Int,Int)
    gen' n p s | next n p `Set.member` s = s
               | otherwise = gen' n (next n p) (next n p `Set.insert` s)
    next n (x, y) = (x * 2 `mod` n, y * 3 `mod` n)

-- solving using longest increasing subsequence problem on the y coordinates 
-- s :: Int -> Int
-- s n = S.length (foldl' s' (S.singleton 0) $ S.fromList $ map snd $ gen n) - 2
--   where
--     s' :: S.Seq Int -> Int -> S.Seq Int
--     s' lis@(l :< is) p 
--       | l <= p = p <| lis
--       | otherwise = S.adjust (const p) index lis
--       where
--         -- todo: binary search
--         index = S.length (S.takeWhileL (>p) lis) - 1

s :: Int -> Int
s n = length $ lis $ map snd $ gen n
 -- todo: understand and optimize.
lis :: Ord a => [a] -> [a]
lis xs = runST $ do
  let lxs = length xs
  pileTops <- newSTArray (min 1 lxs , lxs) []
  i        <- foldM (stack pileTops) 0 xs
  readArray pileTops i
 
stack :: (Integral l, Ord e, Ix l, MArray a [e] u)
      => a l [e] -> l -> e -> u l
stack piles i x = do
  max <- readArray piles i
  if max <= x then writeArray piles (i+1) x
    else do
      j <- bsearch piles x i
      writeArray piles j . (x:) =<< readArray piles (j-1)
  return $ if max <= x then i+1 else i
 
bsearch :: (Integral l, Ord e, Ix l, MArray a [e] u)
        => a l [e] -> e -> l -> u l
bsearch piles x = go 1
  where go lo hi | lo > hi   = return lo
                 | otherwise =
                    do (y:_) <- readArray piles mid
                       if y < x then go (succ mid) hi
                                else go lo (pred mid)
 
                         where mid = (lo + hi) `div` 2
 
newSTArray :: Ix l => (l,l) -> e -> ST o (STArray o l e)
newSTArray = newArray