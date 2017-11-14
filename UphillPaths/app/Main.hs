module Main where

import Data.List
import UphillPaths
import Control.Parallel.Strategies

main :: IO ()
main = print $ foldl' (\acc k -> acc + s (k^5)) 1 ([2..30]::[Int])
