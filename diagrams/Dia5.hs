{-# LANGUAGE NoMonomorphismRestriction, PackageImports #-}
module Dia5 (main, d5) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import DrawUtil
import IconLibrary
import Diagrams.Backend.SVG (renderSVG)

-- For emacs: in image window set M-x auto-revert-mode
-- If reverts are slow, then M-x customize-variable auto-revert-interval

-- MAIN --
main :: IO()
main = mainWith (d5 # frame 0.1 # bg backgroundColor)

saveT5 :: IO ()
saveT5 = renderSVG "t5.svg" (Height 300) framedDiagram

framedDiagram = d5 # frame 0.1 # bg backgroundColor

d5 :: Diagram B R2
d5 = manualLayout <> manualLayout
     # conPortIcon outputColor0 p0Name AppIn y01Name
     # conPortIcon argColor0 p0Name AppL resName 
     # conPortIcon outputColor0 p0Name AppOut y0Name
     # lwL defLineWidth
     # bg backgroundColor

--manualLayout = position (zip (map p2 [(-0.4,0.1), (1,0), (1.5,5), (1.1, -1)]) [y1, t1, f1, x1])
manualLayout = position (zip (map p2 [(-0.2,0), (1,0), (1.5,0), (1, -1)]) [y0, p0, y01, res])

--y01 :: Diagram B R2
--(y01, y01Name) = textInBox "y" "y01"

(y01, y01Name) = yRaw "y01"

res :: Diagram B R2
(res, resName) = result "r"
