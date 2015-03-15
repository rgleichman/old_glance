{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, PackageImports, DeriveDataTypeable #-}
module Main (main) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import qualified "SVGFonts" Graphics.SVGFonts as SVG
import Data.Data (Typeable)

-- For emacs: in image window set M-x auto-revert-mode
-- If reverts are slow, then M-x customize-variable auto-revert-interval

-- MAIN --
main :: IO()
main = mainWith (example # frame 0.1)

-- TOP LEVEL DIAGRAMS --
example :: Diagram B R2
example = e1WithLines

--TODO: figure out if there is a less hacky way to place icons on top of connections
-- CONNECTIONS --
e1WithLines :: Diagram B R2
e1WithLines = manualLayout <> manualLayout
              # yTriFConnect "y1" "t1" "f1"
              # yTriFConnect "y2" "t2" "f2"
              # (lc blue $ connectOutside' (with & arrowHead .~ noHead) "x1" ("t2", TriL))
              # (lc blue $ connectOutside' (with & arrowHead .~ noHead) ("t1", TriL) ("t2", TriR)) #lwL 0.05
              #lwL 0.07
  where
    yTriFConnect y tri f =
      (lc black $ connectOutside' (with & arrowHead .~ noHead ) (tri, TriOut) y)
      . connectNoHead red f (tri, TriIn)
                           
-- ICON PLACEMENT --
--TODO: use an invisible vrule
manualLayout = vcat [yTriF [y1, t1, f1]
                    ,strutY 0.1
                    , yTriF [y2, t2, f2]
                    ,strutY 0.3
                    , x1 # translateX 1]
  --position (zip (map p2 [(-1,0), (1,0), (2,0), (1, -1)]) [y1, t1, f1, x1])
  where
    yTriF ls = position (zip (map p2 [(-0.6,0), (1,0), (2,0)]) ls)
    --yTriF ls = hcat' (with & sep .~ 0.4) ls

-- ICONS --
t1 = applyTriangle blue "t1"
t2 = applyTriangle blue "t2"
f1 = textInBox' "f1"
y1 = textInBox' "y1"
f2 = textInBox' "f2"
y2 = textInBox' "y2"
x1 = textInBox "x" "x1"

-- SYMBOL FUNCTIONS --

data TriPorts = TriL | TriR | TriOut | TriIn
              deriving (Typeable, Eq, Ord, Show)

instance IsName TriPorts

-- ?? Why does line cap style not work here for the vrule?
--applyTriangle :: (Monoid m, Semigroup m, TrailLike (QDiagram b R2 m), IsName n) => Colour Double -> n -> QDiagram b R2 m
applyTriangle color name = (vrule 1 # lwL 0.1 # lc color) <> baseTri # fc lime # lc black # lw veryThin # named name <> decoratePath (scale 0.9 baseTri) (map (\x -> mempty # named (name, x)) [TriR, TriOut, TriL]) <> mempty # named (name, TriIn)
  where
    baseTri = triangle 1 # rotateBy (1/4) # alignR

textInBox ::  IsName n => String -> n -> Diagram B R2
textInBox letters name = t # stroke # fc black # lw none <> (boundingRect t # lw none) # named name
  where
    t = SVG.textSVG' $ SVG.TextOpts letters SVG.bit SVG.INSIDE_H SVG.KERN False 1 1

textInBox' x = textInBox x x

connectNoHead :: (IsName n1, IsName n2) => Colour Double -> n1 -> n2 -> Diagram B R2 -> Diagram B R2
connectNoHead color = connectOutside' (with & arrowHead .~ noHead & headGap .~ negativeMes) # lc color

-- TODO: see if there is a less hacky way to not have a gap between icon and connections
negativeMes = Normalized (-0.01)
