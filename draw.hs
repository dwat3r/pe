{-# LANGUAGE NoMonomorphismRestriction,FlexibleContexts,GADTs #-}
import qualified UphillPaths as U
import UphillPaths (solution,gen,ellista)
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.List ((\\))

fromP :: U.Point -> P2 Double
fromP (U.P (x,y)) = fromIntegral x ^& (fromIntegral y)

main = mainWith (points 10000 :: Diagram B)


--path' = fromVertices . map fromP . toposort

points n = ps n <> strokeP (path n)
  where
    path = fromVertices . map fromP . solution
    ps n = flip atPoints (repeat (circle 0.15 # fc black))
       $ map fromP $ gen n



--pss n = foldl (\acc x -> ) [] $ map (p2 . fromP) $ solution n