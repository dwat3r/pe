module Main where

import Data.List
import UphillPaths
--import Control.Parallel.Strategies

main :: IO ()
--main = print $ sum $ foldl' (\acc k -> acc + s (k^5)) 16 ([17..30]::[Int]) : parMap rpar (\k -> s (k^5)) [1..15]
main = print $ foldl' (\acc k -> acc + s (k^5)) 1 ([2..30]::[Int])
