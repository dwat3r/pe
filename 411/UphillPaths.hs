

import qualified Data.Set as S
import Point
-- gen n = take n $ iterate (\(bx,by) -> (bx * 2 `mod` n,by * 3 `mod` n)) (1,1)

gen :: Int -> S.Set [Point]
gen n = gen' n S.empty

gen' :: Int -> S.Set [Point] -> S.Set [Point]
gen' n s = 
