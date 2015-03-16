{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, PackageImports, DeriveDataTypeable #-}
module Dia1_1 (main, d1_1) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import DrawUtil

-- For emacs: in image window set M-x auto-revert-mode
-- If reverts are slow, then M-x customize-variable auto-revert-interval

-- MAIN --
main :: IO()
main = mainWith (d1_1 # frame 0.1)

d1_1 :: Diagram B R2
d1_1 = manualLayout <> manualLayout
     # conAppIn "t1" "f" funColor0
     # conAppLeft "t1" "x1" argColor0
     # conAppOut "t1" ("t2", AppIn) funColor0
     # conAppLeft "t2" "x2" argColor0
     # conAppOut "t2" "y" outputColor0
     # lwL defLineWidth
                           
-- ICON PLACEMENT --
manualLayout = position (zip (map p2 [(-1.2,0), (0,0), (0, -1.3), (1.5,0), (1.5, -1.3), (2.2,0)]) [y1, t2, x2, t1, x1, f1])

-- ICONS --
t1 = applyTriangle argColor0 "t1"
t2 = applyTriangle argColor0 "t2"
f1 = textInBox' "f"
y1 = textInBox' "y"
x1 = textInBox' "x1"
x2 = textInBox' "x2"

