{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, PackageImports, DeriveDataTypeable #-}
module Dia4_1 (main, d4_1) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import DrawUtil
import IconLibrary

-- For emacs: in image window set M-x auto-revert-mode
-- If reverts are slow, then M-x customize-variable auto-revert-interval

-- MAIN --
main :: IO()
main = mainWith (d4_1 # frame 0.1 # bg backgroundColor)

d4_1 :: Diagram B R2
d4_1 = manualLayout <> manualLayout
     # conPortIcon argColor0 fa0Name AppIn x0Name
     # conPortIcon funColor0 fa0Name AppL f1Name
     # conPortIcon funColor0 fa1Name AppL f2Name
     # conPortIcon outputColor0 fa1Name AppOut y0Name
     # conIcons argColor0 fa0Name AppOut fa1Name AppIn
     # lwL defLineWidth
     # bg backgroundColor
                           
-- ICON PLACEMENT --
manualLayout = position (zip (map p2 [(-1.2,0), (0,0), (0, -1.3), (1.5,0), (1.5, -1.3), (2.2,0)]) [y0, fa1, f2, fa0, f1, x0])
--manualLayout = position (zip (map p2 [(-1.2,0), (0,0), (0, -1.3), (1.5,1), (1.5, -1.3), (2.2,0)]) [y0, t1, x2, t0, x1, f0])
