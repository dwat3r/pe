module Main where

import Data.List (foldl')

import Debug.Trace

main :: IO ()
main = print $ go 1 0

go :: Int -> Int -> Int
go n sum | divisors (sum + n) > 500 = sum + n
         | otherwise = traceShow (divisors (sum + n), sum + n) $ go (n+1) (sum + n)
divisors n = foldl' (*) 1 [2..(getDivisors 0 n 2 n)]

-- amount, accum, currDivisor, original num
getDivisors n acc d nn   | acc == 1 || d == nn `div` 2 = n
                         | acc `mod` d == 0 = getDivisors (n+1) (acc `div` d) d nn
                         | otherwise = getDivisors n acc (d+1) nn