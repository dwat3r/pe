-- todo: integrate hunit, tasty or any testing framework

import Data.List (foldl')
import UphillPaths

main :: IO ()
main = do
    correctnessTest
    --solution



correctnessTest :: IO ()
correctnessTest = putStr $ unlines $ map (show . s) [123]

--solution = print $ foldl' (\acc k -> acc + s (k^5)) 1 [2..10]
