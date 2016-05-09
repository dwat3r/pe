{-# LANGUAGE NoMonomorphismRestriction #-}
import Data.List (nub,delete)
import qualified Data.Map as M
import Data.Array
import Data.Map (Map)
import Debug.Trace (trace)
import Crypto.Number.ModArithmetic (expFast)
-- ellista epites
data Point = P {p :: (Integer,Integer)}
  deriving Ix

instance Show Point where
  show x = show (p x)

instance Eq Point where
  (==) (P (x1,y1)) (P (x2,y2)) = x1 == x2 && y1 == y2

instance Ord Point where
  (<=) (P (x1,y1)) (P (x2,y2)) = x1 <= x2 && y1 <= y2

--pretty print Map
pretty = putStrLn . M.foldlWithKey (\acc k v -> acc ++ show k ++ " => " ++ show v ++ "\n\n") ""

-- ellista epites es minden kulcs egy pont es annak sulya (suly: max hossz (0,0)bol) 
-- kezdetben suly: 0
gen n = nub $ (P (0,0)):(filter (not . eith0) [P (expFast 2 i n,expFast 3 i n) | i<-[0..2*n]]) ++ [P (n,n)]
  where eith0 (P (x,y)) = x == 0 || y == 0

ellista :: [Point] -> Map Point [Point]
ellista xs = foldl (\acc x-> M.insert x (filter (x<=) (delete x xs)) acc) M.empty xs



-- Maxkeres: f(pi) = c pi + max {f(pj)| j<- szomszed(pi)}
points :: Map Point [Point] -> Int
points m = (arr ! (P (0,0))) - 1
  where
    -- lazy array to memo f
    arr = array (fst $ M.findMin m,fst $ M.findMax m) [(p, f p) | p <- M.keys m]
    f p = case M.lookup p m of
      Just ps -> 1 + (maximum $ 0:[arr ! j | j <- ps])
      Nothing -> 0

solution n = do
  --pretty $ ellista $ gen n
  putStrLn $ show $ points $ ellista $ gen n

{-
knapsack items wmax = arr ! wmax
  where
    arr = array (0, wmax) [(w, m w) | w <- [0..wmax]]
    m 0 = 0
    m w = maximum $ 0:[vi + arr ! (w - wi) | (vi, wi) <- items, wi <= w]
-}











