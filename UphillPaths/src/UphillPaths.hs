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
points n = fst $ insertPoints (Node (NodeD (P 0 0) 0) []) False $ gen n

-- algorithm works two ways:
-- make a new branch with p in the tree at some point (use backtracking)
-- extend an existing leaf
-- insertPoints: tree to work on - actual point IF being inserted - list of points - (result tree, actual point IS inserted)
insertPoints :: Tree NodeD -> Bool -> [Point] -> (Tree NodeD, Bool)
insertPoints t _ [] = (t, True)
insertPoints tree@(Node l []) False (p:ps)
    | np l <= p  = (fst $ insertPoints (tree `makeChild` p) False ps, True)
    | otherwise = (tree, False)
insertPoints tree@(Node l cs) False ps = (Node l (tryInsert cs), False)
    where
      tryInsert (n:ns) | inserted = resultChild:ns
                       | False    = n:tryInsert ns
        where
          (resultChild, inserted) = insertPoints n False ps
    

makeChild (Node l@(NodeD _ d) cs) p = Node l (Node (NodeD p $ d + 1) [] : cs)
