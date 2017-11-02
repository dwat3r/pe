import qualified Data.Set as S
import Point

-- gen n = take n $ iterate (\(bx,by) -> (bx * 2 `mod` n,by * 3 `mod` n)) (1,1)

gen :: Int -> S.Set Point
gen n = gen' n (P (1, 1)) $ S.fromList [P (0,0),P (1, 1), P (n,n)]
  where
    gen' :: Int -> Point -> S.Set Point -> S.Set Point
    gen' n p s | next n p `S.member` s = s
               | otherwise = gen' n (next n p) (next n p `S.insert` s)
    next n (P (x, y)) = P (x * 2 `mod` n ,y * 3 `mod` n)
