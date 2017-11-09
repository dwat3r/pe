{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude
import           Diagrams.TwoD.GraphViz

import           Data.GraphViz
import           Data.GraphViz.Commands

hex = mkGraph [0..19]
        (   [ (v, (v+1)`mod`6, ()) | v <- [0..5] ]
         ++ [ (v, v+k, ()) | v <- [0..5], k <- [6,12] ]
         ++ [ (2,18,()), (2,19,()), (15,18,()), (15,19,()), (18,3,()), (19,3,()) ]
        )

main = do
  hex' <- layoutGraph Dot hex
  let hexDrawing :: Diagram B
      hexDrawing = drawGraph
                     (const $ place (circle 19))
                     (\_ p1 _ p2 _ p -> arrowBetween' (opts p) p1 p2)
                     hex'
      opts p = with & gaps .~ 16 & arrowShaft .~ (unLoc . head $ pathTrails p)
  return (hexDrawing # frame 1)