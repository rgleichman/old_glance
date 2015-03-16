{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, PackageImports, DeriveDataTypeable #-}
module Dia3 (main, d3) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import DrawUtil

-- For emacs: in image window set M-x auto-revert-mode
-- If reverts are slow, then M-x customize-variable auto-revert-interval

-- MAIN --
main :: IO()
main = mainWith (d3 # frame 0.1)

--TODO: figure out if there is a less hacky way to place icons on top of connections
-- CONNECTIONS --
d3 :: Diagram B R2
d3 = manualLayout <> manualLayout
              # yAppFConnect "y1" "t1" "f1"
              # yAppFConnect "y2" "t2" "f2"
              # conAppLeft "t2" "x1" argColor0
              # conAppLeft "t1" ("t2", AppR) argColor0
              #lwL defLineWidth
  where
    yAppFConnect y triName f =
      conAppOut triName y outputColor0
      . conAppIn triName f funColor0
                           
-- ICON PLACEMENT --
manualLayout :: Diagram B R2
manualLayout = vcat [yTriF [y1, t1, f1]
                    ,strutY 0.1
                    , yTriF [y2, t2, f2]
                    ,strutY 0.3
                    , x1 # translateX 1]
  where
    yTriF ls = position (zip (map p2 [(-0.6,0), (1,0), (2,0)]) ls)

-- ICONS --
t1 = applyTriangle argColor0 "t1"
t2 = applyTriangle argColor0 "t2"
f1 = textInBox' "f1"
y1 = textInBox' "y1"
f2 = textInBox' "f2"
y2 = textInBox' "y2"
x1 = textInBox "x" "x1"
