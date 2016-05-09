{-# LANGUAGE NoMonomorphismRestriction,DeriveGeneric #-}
module UphillPaths where
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Array
import qualified Data.Set as S
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import GHC.Generics (Generic)
import Data.Hashable
import Data.Set (Set)
import Data.Ord (comparing)
import Data.Tree
import Data.Tree.Pretty
--debug
import Debug.Trace (trace,traceShow)
import Debug.Hood.Observe
import Control.DeepSeq

-- ellista epites
newtype Point = P {p :: (Integer,Integer)}
  deriving (Ix,Generic)

instance Show Point where
  show x = show (p x)

instance Eq Point where
  (==) (P (x1,y1)) (P (x2,y2)) = x1 == x2 && y1 == y2

--TODO: fix ordering
instance Ord Point where
  --(<=) (P (x1,y1)) (P (x2,y2)) = x1 <= x2 && y1 <= y2
  compare (P (x1,y1)) (P (x2,y2)) = case x1 `compare` x2 of
    LT -> case y1 `compare` y2 of
      LT -> LT
      EQ -> LT
      GT -> GT
    EQ -> case y1 `compare` y2 of
      LT -> LT
      EQ -> EQ
      GT -> GT
    GT -> case y1 `compare` y2 of
      LT -> GT
      EQ -> GT
      GT -> GT

instance Hashable Point
instance Observable Point
instance Observable Ordering
instance NFData Point where
  rnf point = deepseq (p point) ()
--pretty print Map
pretty = putStrLn . M.foldlWithKey (\acc k v -> acc ++ show k ++ " => " ++ show v ++ "\n\n") ""


gen :: Integer -> [Point]
gen n = HM.keys $ HM.insert (P (0,0)) 1 $ HM.insert (P (n,n)) 1 (ps (2*n) (P (1,1)) HM.empty) 
  where 
    --ps: ps iteration acc Set
    ps 0 _ s = s
    ps k acc@(P (bx,by)) s = ps (k-1) next (HM.insert acc 1 s)
      where next = P (bx * 2 `mod` n,by * 3 `mod` n)

--ellista epites
ellista :: [Point] -> Map Point [Point]
ellista xs = foldl (\acc x-> M.insert x (filter (x<) xs) acc) M.empty xs



-- new idea:
--start search from (n,n), extend search by extending matrix.
-- if a node found, append it to path


-- Maxkeres: f(pi) = c pi + max {f(pj)| j<- szomszed(pi)}
-- todo: aggregate results which are not needed?
--points :: Integer -> [Point] -> Map Point [Point] -> Int
--points n pl m = (arr ! (P (0,0))) - 2 -- source & dest not counted
points n pl m = f' (P (0,0))
  where
    -- lazy array to memo f
    arr = array (P (0,0),P (n,n)) [(p, f' p) | p <- pl]
    f p = case M.lookup p m of
      Just ps -> p : ( (maximumBy (comparing (\x -> deepseq x $ length x)) $ []:[arr ! j | j <- ps])) 
      Nothing -> []
    dlength p xs ys | null xs && null ys = EQ
                    | True = case length xs `compare` length ys of
      EQ -> dist (P (0,0)) (head xs) `compare` dist (P (0,0)) (head ys)
      ot -> ot
    f' = observe "f" f

dist (P (x1,y1)) (P (x2,y2)) = sqrt (fromIntegral ((x2-x1)^2 + (y2-y1)^2))



--TODO: implement topological sort...
--toposort n = sortBy (comparing $ dist n)


solution n = let ps = gen n in points n ps $ ellista ps
{-
knapsack items wmax = arr ! wmax
  where
    arr = array (0, wmax) [(w, m w) | w <- [0..wmax]]
    m 0 = 0
    m w = maximum $ 0:[vi + arr ! (w - wi) | (vi, wi) <- items, wi <= w]
-}

-- unit testing Ord on Points
test_ordP = map (\x -> (x,P (4,4) <x)) [P (x,y)| x<-[3..5],y<-[3..5]]


main = putStrLn $ drawVerticalTree $ paths 123 (gen 123) $ ellista $ gen 123

--all paths
paths n pl m = f (P (0,0))
  where
    f p = case M.lookup p m of
      Just ps -> Node (show p) [f j | j <- ps] 
      Nothing -> Node (show p) []








