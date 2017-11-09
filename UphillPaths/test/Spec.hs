-- todo: integrate hunit, tasty or any testing framework

import qualified Data.Set as S
import UphillPaths
import Draw

main :: IO ()
main = graphicalTest

correctnessTest :: IO ()
correctnessTest = putStr $ unlines $ zipWith (curry show) (map points [22{-, 123, 10000-}]) [5{-, 14, 48-}]

graphicalTest :: IO ()
graphicalTest = putStr $ draw $ points 22
