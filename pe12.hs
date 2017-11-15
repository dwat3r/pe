module Main where

import Data.List (foldl')
import qualified Data.Map.Strict as M
import Debug.Trace

main :: IO ()
main = print $ go 1 0

go :: Int -> Int -> Int
go n sum | divisors (sum + n) > 500 = sum + n
         | otherwise = go (n+1) (sum + n)

divisors :: Int -> Int
divisors n = getDivisors M.empty n 2 n

-- amount, accum, currDivisor, original num
getDivisors ds acc d nn   | acc == 1 || d == nn `div` 2 = foldl' (*) 1 $ M.elems $ M.map (+1) ds
                          | acc `mod` d == 0 = getDivisors (M.insertWith (+) d 1 ds) (acc `div` d) d nn
                          | otherwise = getDivisors ds acc (d+1) nn