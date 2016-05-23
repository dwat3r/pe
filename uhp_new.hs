gen n = take (2*n) $ iterate (\(bx,by) -> (bx * 2 `mod` n,by * 3 `mod` n)) (1,1)
