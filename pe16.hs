module Main where

main = print $ go $ 2^1000
  where
    go n | n < 10 = n
         | otherwise = n `mod` 10 + go (n `div` 10)