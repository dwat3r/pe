{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
module UphillPaths where

import qualified Data.Set as Set
import qualified Data.Sequence as S
import Data.Sequence ((<|),(|>),(><))
import Data.List

pattern x :< xs <- (S.viewl -> x S.:< xs)

gen :: Int -> [(Int,Int)]
gen n = Set.toList $ gen' n (1, 1) $ Set.fromList [(1,1),(n, n)]
  where
    gen' :: Int -> (Int,Int) -> Set.Set (Int,Int) -> Set.Set (Int,Int)
    gen' n p s | next n p `Set.member` s = s
               | otherwise = gen' n (next n p) (next n p `Set.insert` s)
    next n (x, y) = (x * 2 `mod` n, y * 3 `mod` n)

-- solving using longest increasing subsequence problem on the y coordinates 
s :: Int -> Int
s n = S.length (foldl' s' (S.singleton 0) $ S.fromList $ map snd $ gen n) - 2
  where
    s' :: S.Seq Int -> Int -> S.Seq Int
    s' lis@(l :< is) p 
      | l <= p = p <| lis
      | otherwise = S.adjust (const p) index lis
      where
        -- todo: binary search
        index = S.length (S.takeWhileL (>p) lis) - 1
    
