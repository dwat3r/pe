module UphillPaths where

import Debug.Trace

import qualified Data.Set as S
import Data.Tree

import Point
import Draw
-- todo: rewrite to recursive tree building algorithm

-- node with depth
data NodeD = NodeD { np :: Point, d :: Int }

instance Show NodeD where
  show (NodeD p d) = "(" ++ show p ++ "," ++ show d ++ ")"

gen :: Int -> [Point]
gen n = S.toList $ gen' n (P 1 1) $ S.fromList [P 1 1, P n n]
  where
    gen' :: Int -> Point -> S.Set Point -> S.Set Point
    gen' n p s | next n p `S.member` s = s
               | otherwise = gen' n (next n p) (next n p `S.insert` s)
    next n (P x y) = P (x * 2 `mod` n) (y * 3 `mod` n)


points :: Int -> Tree NodeD
points n = insertPoints (Node (NodeD (P 0 0) 0) []) $ gen n

-- algorithm works two ways:
-- make a new branch with p in the tree at some point (use backtracking)
-- extend an existing leaf

insertPoints :: Tree NodeD -> [Point] -> Tree NodeD
insertPoints tree@(Node l []) (p:ps)
    | np l <= p  = insertPoints (tree `makeChild` p) ps
    | otherwise = undefined -- need to: either branch or try insert at different leaf
insertPoints tree@(Node l cs) p = Node l (map insertPoints cs)
    

makeChild (Node l@(NodeD _ d) cs) p = Node l (Node (NodeD p d + 1) [] : cs)
