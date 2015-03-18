{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, PackageImports, DeriveDataTypeable #-}
module Dia1_1 (main, d1_1) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import DrawUtil
import IconLibrary

-- For emacs: in image window set M-x auto-revert-mode
-- If reverts are slow, then M-x customize-variable auto-revert-interval

-- MAIN --
main :: IO()
main = mainWith (d1_1 # frame 0.1 # bg backgroundColor)

d1_1 :: Diagram B R2
d1_1 = manualLayout <> manualLayout
     # conPortIcon funColor0 t0Name AppIn f0Name
     # conPortIcon argColor0 t0Name AppL x1Name
     # conPortIcon argColor0 t1Name AppL x2Name
     # conPortIcon outputColor0 t1Name AppOut y0Name
     # conIcons funColor0 t0Name AppOut t1Name AppIn
     # lwL defLineWidth
     # bg backgroundColor
                           
-- ICON PLACEMENT --
manualLayout = position (zip (map p2 [(-1.2,0), (0,0), (0, -1.3), (1.5,0), (1.5, -1.3), (2.2,0)]) [y0, t1, x2, t0, x1, f0])
--manualLayout = position (zip (map p2 [(-1.2,0), (0,0), (0, -1.3), (1.5,1), (1.5, -1.3), (2.2,0)]) [y0, t1, x2, t0, x1, f0])
