{-# LANGUAGE NoMonomorphismRestriction, PackageImports #-}
module Dia6 (main, d6) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import DrawUtil
import IconLibrary
import Diagrams.Backend.SVG (renderSVG)

-- For emacs: in image window set M-x auto-revert-mode
-- If reverts are slow, then M-x customize-variable auto-revert-interval

-- f = (\q -> q)
-- y = f x

-- MAIN --
main :: IO()
main = mainWith framedDiagram

saveT6 :: IO ()
saveT6 = renderSVG "t6.svg" (Height 200) framedDiagram

framedDiagram = d6 # frame 0.1 # bg backgroundColor

d6 :: Diagram B R2
d6 = manualLayout <> manualLayout
     # conPortIcon lambdaColor t0Name AppIn f0Name
     # conPortIcon argColor0 t0Name AppL resName 
     # conPortIcon outputColor0 t0Name AppOut y0Name
     # conIconIcon outputColor0 resName y02Name
     # conPortIcon argColor0 t0Name AppR x0Name
     # lwL defLineWidth
     # bg backgroundColor

manualLayout = position (zip (map p2 [(-1.2,0), (0,0), (0.7,0), (0, -1), (0,-2), (0, 1)]) [y0, t0, f0, res, y02, x0])

--y01 :: Diagram B R2
--(y01, y01Name) = textInBox "y" "y01"

(y01, y01Name) = yRaw "y01"
(y02, y02Name) = yRaw "y02"

res :: Diagram B R2
(res, resName) = result "r"
