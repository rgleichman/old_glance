{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, DeriveDataTypeable #-}
module DrawUtil
       (funColor0
         ,argColor0
         ,outputColor0
         ,defLineWidth
         ,ApplyPorts(..)
         ,applyTriangle
         ,conAppLeft
         ,conAppRight
         ,conAppOut
         ,conAppIn
         ,funApply
         ,textInBox
         ,textInBox'
         )
       where

import Diagrams.Prelude
import Graphics.SVGFonts as SVG
import Data.Data (Typeable)
import Diagrams.Backend.SVG.CmdLine

-- COLO(U)RS --
funColor0 :: (Floating a, Ord a) => Colour a
funColor0 = red
argColor0 :: (Floating a, Ord a) => Colour a
argColor0 = blue
applyTriColor :: (Floating a, Ord a) => Colour a
applyTriColor = lime
outputColor0 :: Num a => Colour a
outputColor0 = black
funApplyFillColor :: (Floating a, Ord a) => Colour a
funApplyFillColor = cyan

-- CONSTANTS --
defLineWidth :: Fractional a => a
defLineWidth = 0.07

-- APPLY TRIANGLE --
data ApplyPorts = AppL | AppR | AppOut | AppIn
              deriving (Typeable, Eq, Ord, Show)

instance IsName ApplyPorts

applyTriangle :: (Monoid m, Semigroup m, TrailLike (QDiagram b R2 m), IsName t) => Colour Double -> t -> QDiagram b R2 m
applyTriangle color name = (vrule size # lwL (0.1*size) # lc color) <> baseTri # fc applyTriColor # lc black # lw veryThin # named name <> decoratePath (scale 0.9 baseTri) (map (\x -> mempty # named (name, x)) [AppR, AppOut, AppL]) <> mempty # named (name, AppIn)
  where
    baseTri = triangle size # rotateBy (1/4) # alignR
    size = 0.7

connectAppArgOrOut :: (Renderable (Path R2) b, IsName t, IsName t1, IsName n2) => t1 -> t -> n2 -> Colour Double -> Diagram b R2 -> Diagram b R2
connectAppArgOrOut port triName dest color = lc color . connectOutside' (with & arrowHead .~ noHead) (triName, port) dest
conAppLeft = connectAppArgOrOut AppL
conAppRight = connectAppArgOrOut AppR
conAppOut = connectAppArgOrOut AppOut
conAppIn :: (Renderable (Path R2) b, IsName t, IsName n2) => t -> n2 -> Colour Double -> Diagram b R2 -> Diagram b R2
conAppIn triName dest color = lc color . connectOutside' (with & arrowHead .~ noHead & headGap .~ negativeMes) (triName, AppIn) dest
  where
    -- TODO: see if there is a less hacky way to not have a gap between icon and connections
    negativeMes = Normalized (-0.01)

-- FUNCTION APPLY --
funApply :: (Monoid m, Semigroup m, TrailLike (QDiagram b R2 m), IsName t) => Colour Double -> t -> QDiagram b R2 m
funApply color name = (vrule (radius'*2) # lwL (radius'*0.2) # lc color) <> decoratePath verts (map makePort [AppOut, AppR, AppL, AppIn]) <> semiCirc
  where
    semiCirc = wedge radius' (1/4 @@ turn) (-1/4 @@ turn) # fc funApplyFillColor # lc black # lw veryThin
    radius' = 0.35
    insideScale = 0.9
    portDist = radius'*insideScale
    makePort subName = mempty # named (name, subName)
    verts = fromVertices $ map p2 [(-portDist,0),(0,portDist),(0,-portDist),(0,0)]

-- TEXT --
textInBox :: (Renderable (Path R2) b, IsName n) => String -> n -> Diagram b R2
textInBox letters name = t # stroke # fc outputColor0 # lw none <> (boundingRect t # lw none) # named name
  where
    t = SVG.textSVG' $ SVG.TextOpts letters SVG.bit SVG.INSIDE_H SVG.KERN False 1 1

textInBox' :: Renderable (Path R2) b => String -> Diagram b R2
textInBox' x = textInBox x x

-- MAIN --
main :: IO()
main = mainWith (test # frame 0.1)

test :: Diagram B R2
test = funApply argColor0 "test"
