{-# LANGUAGE NoMonomorphismRestriction, PackageImports #-}
module Dia7 (main, d7) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import DrawUtil
import IconLibrary
import Diagrams.Backend.SVG (renderSVG)

-- For emacs: in image window set M-x auto-revert-mode
-- If reverts are slow, then M-x customize-variable auto-revert-interval

-- y = (\_ -> ())

-- MAIN --
main :: IO()
main = mainWith framedDiagram

saveT7 :: IO ()
saveT7 = renderSVG "t7.svg" (Height 200) framedDiagram

framedDiagram = d7 # frame 0.1 # bg backgroundColor

d7 :: Diagram B R2
d7 = manualLayout <> manualLayout
     # conPortIcon lambdaColor pOpenName AppIn y01Name
     # scopePortIcon pOpenName AppL tuName
     # conIconIcon outputColor0 resName tuName
     # conPortIcon lambdaColor pOpenName AppOut y0Name
     # conIconIcon lambdaColor resName y02Name
     # lwL defLineWidth
     # bg backgroundColor

manualLayout = position (zip (map p2 [(-1.2,0), (0,0), (0.5,0), (0,-2), (0, -3), (0,-4)]) [y0, pOpen, y01, tu, res, y02])

--y01 :: Diagram B R2
--(y01, y01Name) = textInBox "y" "y01"

pOpen :: Diagram B R2
(pOpen, pOpenName) = funParamNoColor "p0"
(y01, y01Name) = yRaw "y01"
(y02, y02Name) = yRaw "y02"

res :: Diagram B R2
(res, resName) = result "r"

tu :: Diagram B R2
(tu, tuName) = textInBox' "()"
