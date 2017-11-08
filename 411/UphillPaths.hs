module UphillPaths where

import Debug.Trace

import qualified Data.Set as S
import Data.Tree
import Data.Tree.Zipper

import Point
import Draw

data Frontier = F {p :: TreePos Full Point, depth :: Int}
  deriving Eq

instance Ord Frontier where
  compare (F _ i) (F _ j) = i `compare` j

instance Show Frontier where
  show (F p d) = show p ++ ", " ++ show d

gen :: Int -> S.Set Point
gen n = gen' n (P 1 1) $ S.fromList [P 1 1, P n n]
  where
    gen' :: Int -> Point -> S.Set Point -> S.Set Point
    gen' n p s | next n p `S.member` s = s
               | otherwise = gen' n (next n p) (next n p `S.insert` s)
    next n (P x y) = P (x * 2 `mod` n) (y * 3 `mod` n)


points :: Int -> [S.Set Frontier]
points n = scanl processPoints frontStart $ S.toList $ gen n
--where
rootNode = Node (P 0 0) []
frontStart = S.singleton $ F (fromTree rootNode) 0


processPoints :: S.Set Frontier -> Point -> S.Set Frontier
processPoints frontier p = foldl (processFrontier p) S.empty frontier


processFrontier :: Point -> S.Set Frontier -> Frontier -> S.Set Frontier
processFrontier p newFrontier f@(F fp d) | label fp <= p = {- traceShow  fp $ -} (insertPoint f p) `S.insert` newFrontier
                                         | otherwise = newFrontier
insertPoint (F fp d) p = F (insert (Node p []) (children fp)) $ d + 1
