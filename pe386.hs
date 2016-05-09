import Data.List

solution = undefined

factor n = go n 2 
  where
    go 1 _ = []
    go n i | n `mod` i == 0 = i:go (n `div` i) i
           | otherwise      = go n (nextPrime i)
    nextPrime n = head $ dropWhile (not . isPrime) [(n+1)..]

isPrime :: (Integral a) => a -> Bool
isPrime n | n < 4 = n > 1
isPrime n = all ((/=0).mod n) $ 2:3:[x + i | x <- [6,12..s], i <- [-1,1]]
            where s = floor $ sqrt $ fromIntegral n
