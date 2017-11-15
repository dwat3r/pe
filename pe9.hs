module Main where

main = (\(a,b,c) -> a*b*c) $ head [(a,b,(1000-a-b))| a <- [1..500], b <- [1..500], a^2 + b^2 == (1000 - a - b)^2]