{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, PackageImports #-}
module Dia2 (main, d2) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import DrawUtil
import IconLibrary

-- For emacs: in image window set M-x auto-revert-mode
-- If reverts are slow, then M-x customize-variable auto-revert-interval

-- MAIN --
main :: IO()
main = mainWith (d2 # frame 0.1 # bg backgroundColor)


d2 :: Diagram B R2
d2 = manualLayout <> manualLayout
     # conPortIcon argColor0 fa0Name AppIn x0Name
     # conPortIcon funColor0 fa0Name AppL f0Name 
     # conPortIcon outputColor0 fa0Name AppOut y0Name
     # lwL defLineWidth
     # bg backgroundColor

--manualLayout = position (zip (map p2 [(-0.4,0.1), (1,0), (1.5,0.1), (1.1, -1)]) [y1, t1, f1, x1])
manualLayout = position (zip (map p2 [(-1,0), (0,0), (0.5,0), (0, -1.2)]) [y0, fa0, x0, f0])
