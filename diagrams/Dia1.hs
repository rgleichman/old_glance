{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, PackageImports #-}
module Main (main, d1) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import DrawUtil

-- For emacs: in image window set M-x auto-revert-mode
-- If reverts are slow, then M-x customize-variable auto-revert-interval

-- MAIN --
main :: IO()
main = mainWith (d1 # frame 0.1)

d1 :: Diagram B R2
d1 = manualLayout <> manualLayout
     # conAppIn "t1" "f1" funColor0
     # conAppLeft "t1" "x1" argColor0
     # conAppOut "t1" "y1" outputColor0
     # lwL defLineWidth

--manualLayout = position (zip (map p2 [(-0.4,0.1), (1,0), (1.5,0.1), (1.1, -1)]) [y1, t1, f1, x1])
manualLayout = position (zip (map p2 [(-0.2,0), (1,0), (1.5,0), (1, -1)]) [y1, t1, f1, x1])

-- ICONS --
t1 = applyTriangle argColor0 "t1"
f1 = textInBox "f" "f1"
y1 = textInBox "y" "y1"
x1 = textInBox "x" "x1"
