{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, PackageImports #-}
module Main (main) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import qualified "SVGFonts" Graphics.SVGFonts as SVG

-- For emacs: in image window set M-x auto-revert-mode
-- If reverts are slow, then M-x customize-variable auto-revert-interval

--TODO: figure out how to not have the lines and text overlap with round end-caps

-- MAIN --
main :: IO()
main = mainWith (example # frame 0.1)

-- TOP LEVEL DIAGRAMS --
example :: Diagram B R2
example = e1WithLines
          
e1WithLines :: Diagram B R2
e1WithLines = manualLayout #
              connectNoHead red "f1" "t1in" #
              (lc black $ connectOutside' (with & arrowHead .~ noHead) "t1out" "y1") #
              (lc blue $ connectOutside' (with & arrowHead .~ noHead & tailGap .~ small) "x1" "t1l") #lwL 0.1

--manualLayout = position (zip (map p2 [(-0.4,0.1), (1,0), (1.5,0.1), (1.1, -1)]) [y1, t1, f1, x1])
manualLayout = position (zip (map p2 [(-0.4,0), (1,0), (1.5,0), (1, -1)]) [y1, t1, f1, x1])

-- ICONS --
t1 = applyTriangle blue "t1"
f1 = textInBox "f" "f1"
y1 = textInBox "y" "y1"
x1 = textInBox "x" "x1"

-- SYMBOL FUNCTIONS --

-- ?? Why does line cap style not work here for the vrule?
--applyTriangle :: (Monoid m, Semigroup m, TrailLike (QDiagram b R2 m), IsName n) => Colour Double -> n -> QDiagram b R2 m
applyTriangle color name = (vrule 1 # lwL 0.1 # lc color) <> baseTri # fc lime # lw none # named name <> decoratePath baseTri (map (\x -> mempty # named (name ++ x)) ["r", "out", "l"]) <> mempty # named (name ++ "in")
  where
    baseTri = triangle 1 # rotateBy (1/4) # alignR

textInBox ::  IsName n => String -> n -> Diagram B R2
textInBox letters name = t # stroke # fc black # lw none <> (boundingRect t # lw none) # named name
  where
    t = SVG.textSVG' $ SVG.TextOpts letters SVG.bit SVG.INSIDE_H SVG.KERN False 1 1

connectNoHead :: (IsName n1, IsName n2) => Colour Double -> n1 -> n2 -> Diagram B R2 -> Diagram B R2
connectNoHead color = connectOutside' (with & arrowHead .~ noHead & gaps .~ small) # lc color
