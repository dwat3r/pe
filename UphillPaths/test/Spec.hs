-- todo: integrate hunit, tasty or any testing framework

import Data.List (foldl')
import UphillPaths
import Control.Parallel.Strategies

main :: IO ()
main = do
    --correctnessTest
    solution



correctnessTest :: IO ()
correctnessTest = putStr $ unlines $ map (show . s) [123]

solution = print $ sum $ map (\k -> s (k^5)) [1..30]
