module Point where

data Point = P {x :: Int, y :: Int}

instance Show Point where
  show (P x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

instance Eq Point where
  (==) (P x1 y1) (P x2 y2) = x1 == x2 && y1 == y2

instance Ord Point where
  (<=) (P x1 y1) (P x2 y2) = x1 <= x2 && y1 <= y2
