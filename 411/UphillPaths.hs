module UphillPaths where

import qualified Data.Set as S
import Point
import Data.Tree
import Data.Tree.Zipper

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


points n = foldl processPoints (root, frontStart) ps
  where
    ps = gen n
    root = Node (P 0 0) []
    frontStart = S.fromList [F (fromTree root) 0]
    processPoints ((tree, frontier), p) = undefined--(foldl processFrontier (newElem, newBranch) frontier
    processFrontier = undefined

--insert
