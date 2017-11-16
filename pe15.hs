module Main where

import Data.List (foldl')

main = print $ fac 40 `div` (fac 20 ^ 2)

fac n = foldl' (*) 1 [2..n]