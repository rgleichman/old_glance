{-# LANGUAGE NoMonomorphismRestriction #-}
module Dia3 (main, d3) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import DrawUtil
import IconLibrary

-- For emacs: in image window set M-x auto-revert-mode
-- If reverts are slow, then M-x customize-variable auto-revert-interval

-- MAIN --
main :: IO()
main = mainWith (d3 # frame 0.1 # bg backgroundColor)

--TODO: figure out if there is a less hacky way to place icons on top of connections
-- CONNECTIONS --
d3 :: Diagram B R2
d3 = manualLayout <> manualLayout
              # yAppFConnect y1Name t0Name f1Name
              # yAppFConnect y2Name t1Name f2Name
              # conPortIcon argColor0 t1Name AppL x0Name
              # conIcons argColor0 t0Name AppL t1Name AppR
              #lwL defLineWidth
              #bg backgroundColor
  where
    yAppFConnect y triName f =
      conPortIcon outputColor0 triName AppOut y
      . conPortIcon funColor0 triName AppIn f
                           
-- ICON PLACEMENT --
manualLayout :: Diagram B R2
manualLayout = vcat [yTriF [y1, t0, f1]
                    ,strutY 0.1
                    , yTriF [y2, t1, f2]
                    ,strutY 0.3
                    , x0 # translateX 1]
  where
    yTriF ls = position (zip (map p2 [(-0.6,0), (1,0), (2,0)]) ls)
