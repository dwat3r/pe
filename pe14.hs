module Main where

import Data.List
import Data.Ord

collatz 1 = 1
collatz n | n `mod` 2 == 0 = 1 + collatz (n `div` 2)
          | otherwise      = 1 + collatz (3*n + 1)

main = print $ maximumBy (comparing snd) $ map (\x -> (x, collatz x)) [1..(10^6)]