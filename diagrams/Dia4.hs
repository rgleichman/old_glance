{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module Dia4 (main, d4) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import DrawUtil
import IconLibrary

-- For emacs: in image window set M-x auto-revert-mode
-- If reverts are slow, then M-x customize-variable auto-revert-interval

-- MAIN --
main :: IO()
main = mainWith (d4 # frame 0.1 # bg backgroundColor)

--TODO: figure out if there is a less hacky way to place icons on top of connections
-- CONNECTIONS --
d4 :: Diagram B R2
d4 = manualLayout <> manualLayout
              # yTriFConnect y1Name fa0Name x1Name
              # yTriFConnect y2Name fa1Name x2Name
              # conPortIcon funColor0 fa1Name AppL f0Name
              # conIcons funColor0 fa0Name AppL fa1Name AppR
              #lwL defLineWidth
              # bg backgroundColor
  where
    yTriFConnect y triName f =
      conPortIcon outputColor0 triName AppOut y
      . conPortIcon argColor0 triName AppIn f

                           
-- ICON PLACEMENT --
manualLayout :: Diagram B R2
manualLayout = vcat [yTriF [y1, fa0, x1]
                    ,strutY 0.1
                    , yTriF [y2, fa1, x2]
                    ,strutY 0.3
                    , f0 # translateX 1]
  where
    yTriF ls = position (zip (map p2 [(-0.6,0), (1,0), (2,0)]) ls)
