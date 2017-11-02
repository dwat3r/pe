module Point where

newtype Point = P {p :: (Int, Int)}

instance Show Point where
  show x = show (p x)

instance Eq Point where
  (==) (P (x1,y1)) (P (x2,y2)) = x1 == x2 && y1 == y2

--TODO: fix ordering
instance Ord Point where
  (<=) (P (x1,y1)) (P (x2,y2)) = x1 <= x2 && y1 <= y2
