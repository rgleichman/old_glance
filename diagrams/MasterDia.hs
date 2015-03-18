{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, PackageImports #-}
module MasterDia (main, masterDia) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import DrawUtil
import Dia1 (d1)
import Dia1_1 (d1_1)
import Dia2 (d2)
import Dia3 (d3)
import Dia4 (d4)


-- For emacs: in image window set M-x auto-revert-mode
-- If reverts are slow, then M-x customize-variable auto-revert-interval

-- MAIN --
main :: IO()
main = mainWith (masterDia # frame 0.1)

masterDia :: Diagram B R2
masterDia = vcat'
            (with & sep .~ 0.5)
            [d1, d1_1, d2, d3, d4]
            # bg backgroundColor
