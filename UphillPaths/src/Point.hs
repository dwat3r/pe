module Point where

data Point = P {x :: !Int, y :: !Int}
  deriving Eq

instance Show Point where
  show (P x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

instance Ord Point where
  compare (P x1 y1) (P x2 y2) = case x1 `compare` x2 of
    EQ -> y1 `compare` y2
    c -> c                             
