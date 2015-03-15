{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, PackageImports #-}
module Main (main) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import qualified "SVGFonts" Graphics.SVGFonts as SVG

-- For emacs: in image window set M-x auto-revert-mode
-- If reverts are slow, then M-x customize-variable auto-revert-interval

--TODO: figure out how to not have the lines and text overlap with round end-caps
-- TODO: Don't use angles (connectPerim') for triangle

-- MAIN --
main :: IO()
main = mainWith example

-- TOP LEVEL DIAGRAMS --
example :: Diagram B R2
example = e1WithLines
          
e1WithLines :: Diagram B R2
e1WithLines = manualLayout #
              connectNoHead red "f1" "t1" #
              (lc black $ connectPerim' (with & arrowHead .~ noHead) "t1" "y1" (1/2 @@ turn) (0 @@ turn)) #
              (lc blue $ connectPerim' (with & arrowHead .~ noHead) "x1" "t1" (1/4 @@ turn) (-91 @@ deg)) #lwL 0.1

manualLayout = position (zip (map p2 [(-0.5,0.1), (1,0), (2,-0.1), (1.2, -1)]) [y1, t1, f1, x1])

-- ICONS --
t1 = applyTriangle blue "t1"
f1 = textInBox "f" "f1"
y1 = textInBox "y" "y1"
x1 = textInBox "x" "x1"

-- SYMBOL FUNCTIONS --

-- ?? Why does line cap style not work here for the vrule?
applyTriangle color name = (vrule 1 # lwL 0.1 # lc color) <> triangle 1 # fc green # lw none # rotateBy (1/4) # alignR  # named name

textInBox ::  IsName n => String -> n -> Diagram B R2
textInBox letters name = t # stroke # fc black # lw none <> (boundingRect t # lw none) # named name
  where
    t = SVG.textSVG letters 1

connectNoHead :: (IsName n1, IsName n2) => Colour Double -> n1 -> n2 -> Diagram B R2 -> Diagram B R2
connectNoHead color = connectOutside' (with & arrowHead .~ noHead) # lc color
