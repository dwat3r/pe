module Main where

import qualified Data.Map.Strict as M
import Data.Maybe

numbers1 = M.fromList [(0, 0), -- zero
                       (1, 3), -- one
                       (2, 3), -- two
                       (3, 5), -- three
                       (4, 4), -- four
                       (5, 4), -- five
                       (6, 3), -- six
                       (7, 5), -- seven
                       (8, 5), -- eight
                       (9, 4), -- nine
                       (10, 3), -- ten
                       (11, 6), -- eleven
                       (12, 6), -- twelve
                       (13, 8), -- thirteen
                       (14, 8), -- fourteen
                       (15, 7), -- fifteen
                       (16, 7), -- sixteen
                       (17, 9), -- seventeen
                       (18, 8), -- eighteen
                       (19, 8)  -- nineteen
                       ]

numbers2 = M.fromList [(0, 0), -- zero
                       (2, 6), -- twenty
                       (3, 6), -- thirty
                       (4, 6), -- fourty
                       (5, 5), -- fifty
                       (6, 5), -- sixty
                       (7, 7), -- seventy
                       (8, 6), -- eighty
                       (9, 6)  -- ninety
                       ]

main = print $ map (\n -> (n, go n)) [1..1000]

go 1000 = 11 -- one thousand
go n | n < 20 = fromJust (M.lookup n numbers1)
     | otherwise = case digits n of
        (o:t:h:_) -> if t >= 2 then fromJust (M.lookup h numbers1) + 10 + fromJust (M.lookup t numbers2) + fromJust (M.lookup o numbers1)
                     else if t > 0 || o > 0 then fromJust (M.lookup h numbers1) + 10 + fromJust (M.lookup (t*10 + o) numbers1)
                     else if t == 0 && o > 0 then fromJust (M.lookup h numbers1) + 10 + fromJust (M.lookup (t*10 + o) numbers1)
                     else fromJust (M.lookup h numbers1) + 7
        (o:t:_)   -> fromJust (M.lookup t numbers2) + fromJust (M.lookup o numbers1)

digits n | n < 10 = [n]
         | True   = n `mod` 10 : (digits $ n `div` 10)