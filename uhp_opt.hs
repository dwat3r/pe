module Main where

import Control.Parallel.Strategies
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Function.Memoize


s n = points n

main = print $ sum (map (\k -> s (k^5)) [1..30] `using` parTraversable rpar) 

--to be optimised
gen :: Int -> [(Int,Int)]
gen n = (0,0): (HM.keys $ HM.insert (n,n) 1 (ps (2*n) (1,1) HM.empty))
  where 
    --ps: ps iteration acc Set
    ps 0 _ s = s
    ps k acc@(bx,by) s = ps (k-1) next (HM.insert acc 1 s)
      where next = (bx * 2 `mod` n,by * 3 `mod` n)

points :: Int -> Int
points n = f (0,0) - 2
  where
    f p = 1 + (maximum $ 0: map f (filter (pls p) ps))
    ps = gen n
    pls (x1,y1) (x2,y2) = x1 <= x2 && y1 < y2 || x1 < x2 && y1 <= y2

--new things to happen now
--todo: create function which tells how many points should we generate.
gen' n = take (2*n) $ iterate (\(bx,by) -> (bx * 2 `mod` n,by * 3 `mod` n)) (1,1)

gen'' n | n `mod` 2 == 0 && n `mod` 3 == 0 = take nx    $ gen' n
        | n `mod` 2 == 0 || n `mod` 3 == 0 = take mx    $ gen' n
        | otherwise                        = take (n-1) $ gen' n
  where
    mx = n `div` 2 + n `div` 3 + 1
    nx = max (n `div` 2) (n `div` 3)


gen2 n = iterate (\x -> x * 2 `mod` n) 1
gen3 n = iterate (\x -> x * 3 `mod` n) 1