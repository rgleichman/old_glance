{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, PackageImports, DeriveDataTypeable #-}
module Dia4 (main, d4) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import DrawUtil

-- For emacs: in image window set M-x auto-revert-mode
-- If reverts are slow, then M-x customize-variable auto-revert-interval

-- MAIN --
main :: IO()
main = mainWith (d4 # frame 0.1)

--TODO: figure out if there is a less hacky way to place icons on top of connections
-- CONNECTIONS --
d4 :: Diagram B R2
d4 = manualLayout <> manualLayout
              # yTriFConnect "y1" "t1" "x1"
              # yTriFConnect "y2" "t2" "x2"
              # conAppLeft "t2" "f1" funColor0
              # conAppLeft "t1" ("t2", AppR) funColor0
              #lwL defLineWidth
  where
    yTriFConnect y triName f =
      conAppOut triName y outputColor0
      . conAppIn triName f argColor0
                           
-- ICON PLACEMENT --
manualLayout :: Diagram B R2
manualLayout = vcat [yTriF [y1, t1, x1]
                    ,strutY 0.1
                    , yTriF [y2, t2, x2]
                    ,strutY 0.3
                    , f1 # translateX 1]
  where
    yTriF ls = position (zip (map p2 [(-0.6,0), (1,0), (2,0)]) ls)

-- ICONS --
t1 = funApply funColor0 "t1"
t2 = funApply funColor0 "t2"
f1 = textInBox "f" "f1"
y1 = textInBox' "y1"
y2 = textInBox' "y2"
x1 = textInBox' "x1"
x2 = textInBox' "x2"
