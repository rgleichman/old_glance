{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, DeriveDataTypeable #-}
module DrawUtil
       (funColor0
       ,argColor0
       ,outputColor0
       ,backgroundColor
       ,lambdaColor
       ,defLineWidth
       ,ApplyPorts(..)
       ,applyTriangle
       ,funParam
       ,funApply
       ,result
       ,textInBox
       ,textInBox'
       ,conIcons
       ,conIconPort
       ,conPortIcon
       ,conIconIcon
       ,Icon(..)
       )
       where

import Diagrams.Prelude
import Graphics.SVGFonts as SVG
import Data.Data (Typeable)
import Diagrams.Backend.SVG.CmdLine
import Data.Colour.SRGB (sRGB24)

-- COLO(U)RS --
colorScheme :: (Floating a, Ord a) => ColorStyle a
colorScheme = colorOnBlackScheme

data ColorStyle a = ColorStyle {funC0 :: Colour a
                               ,argC0 :: Colour a
                               ,appTriC :: Colour a
                               ,outC :: Colour a
                               ,funAppFC :: Colour a
                               ,bgC :: Colour a
                               ,textC :: Colour a
                               ,resC :: Colour a
                               ,lamC :: Colour a
                               ,hasOutlines :: Bool
                                }

normalColorScheme :: (Floating a, Ord a) => ColorStyle a
normalColorScheme =
  ColorStyle {funC0 = red
             ,argC0 = blue
             ,appTriC = lime
             ,outC = black
             ,funAppFC = brightPurple
             ,bgC = white
             ,textC = black
             ,resC = black
             ,lamC = purple
             ,hasOutlines = True}
  where
    brightPurple = sRGB24 183 0 255

blackColorScheme :: (Floating a, Ord a) => ColorStyle a
blackColorScheme =
  ColorStyle {funC0 = black
             ,argC0 = black
             ,appTriC = black
             ,outC = black
             ,funAppFC = black
             , bgC = white
             , textC = black
             ,resC = black
             ,lamC = black
             ,hasOutlines = False}

colorOnBlackScheme :: (Floating a, Ord a) => ColorStyle a
colorOnBlackScheme =
  ColorStyle {funC0 = lightRed
             ,argC0 = bluishCyan
             ,appTriC = yellowishGreen
             ,outC = reddishWhite
             ,funAppFC = yellow
             ,bgC = black
             ,textC = white
             ,resC = lightYellwGreen
             ,lamC = lightMagenta
             ,hasOutlines = False}
  where
    --lightBlue = sRGB24 51 105 255
    --lightBlue = sRGB24 94 169 255
    --lightRed = sRGB24 255 51 51
    lightRed = sRGB24 255 93 61
    bluishCyan = sRGB24 0 230 255
    reddishWhite = sRGB24 255 232 230
    yellowishGreen = sRGB24 191 255 0
    lightYellwGreen = sRGB24 232 255 115
    lightMagenta = sRGB24 255 134 255
  
funColor0 :: (Floating a, Ord a) => Colour a
funColor0 = funC0 colorScheme
argColor0 :: (Floating a, Ord a) => Colour a
argColor0 = argC0 colorScheme
applyTriColor :: (Floating a, Ord a) => Colour a
applyTriColor = appTriC colorScheme
outputColor0 :: (Floating a, Ord a) => Colour a
outputColor0 = outC colorScheme
funApplyFillColor :: (Floating a, Ord a) => Colour a
funApplyFillColor = funAppFC colorScheme
backgroundColor :: (Floating a, Ord a) => Colour a
backgroundColor = bgC colorScheme
lambdaColor :: (Floating a, Ord a) => Colour a
lambdaColor = lamC colorScheme

-- CONSTANTS --
defLineWidth :: Fractional a => a
defLineWidth = 0.13
gapSize :: Fractional (Scalar v) => Measure v
gapSize = Local 0.05
outlineScaleFactor :: Fractional a => a
outlineScaleFactor = 0.07

-- APPLY TRIANGLE --
data ApplyPorts = AppL | AppR | AppOut | AppIn
              deriving (Typeable, Eq, Ord, Show, Enum)

instance IsName ApplyPorts

applyTriangle :: (Monoid m, Semigroup m, TrailLike (QDiagram b R2 m), IsName t) => Colour Double -> t -> (QDiagram b R2 m, (t, Icon))
applyTriangle = argumentOrParam 1 applyTriColor

funParam :: (Monoid m, Semigroup m, TrailLike (QDiagram b R2 m), IsName t) => Colour Double -> t -> (QDiagram b R2 m, (t, Icon))
funParam = argumentOrParam 2.5 backgroundColor

argumentOrParam :: (Monoid m, Semigroup m, TrailLike (QDiagram b R2 m), IsName t) => Double -> Colour Double -> Colour Double -> t -> (QDiagram b R2 m, (t, Icon))
argumentOrParam outlineScale insideColor color name = (name |> diagram, (name, Apply))
  where
    diagram = (vrule (size + (outlineWidth * squarRootThree)) # lwL defLineWidth # lc color)
              <> baseTri # fc insideColor # lc outlineColor # lwL outlineWidth # named name
              <> decoratePath (scale 0.9 baseTri) (map (\x -> mempty # named x) [AppR, AppOut, AppL]) <> mempty # named AppIn
    baseTri = triangle size # rotateBy (1/4) # alignR
    size = 0.7
    outlineWidth = outlineScale*outlineScaleFactor*size :: Double
    squarRootThree = 1.732
    outlineColor = applyTriColor

-- FUNCTION APPLY --
funApply :: (Semigroup m, TrailLike (QDiagram b R2 m), IsName t) => Colour Double -> t -> (QDiagram b R2 m, (t, Icon))
funApply color name = (name |> diagram, (name, Apply))
  where
    diagram = (vrule (2*radius' + (1.1 *outlineWidth)) # lwL defLineWidth # lc color) <> decoratePath verts (map makePort [AppOut, AppR, AppL, AppIn]) <> semiCirc
    semiCirc = wedge radius' (1/4 @@ turn) (-1/4 @@ turn) # fc funApplyFillColor # lc outlineColor # lwL outlineWidth
    radius' = 0.35
    insideScale = 0.9
    portDist = radius'*insideScale
    makePort subName = mempty # named subName
    verts = fromVertices $ map p2 [(-portDist,0),(0,portDist),(0,-portDist),(0,0)]
    outlineColor = if hasOutlines colorScheme then black else funApplyFillColor
    outlineWidth = 2 * outlineScaleFactor * radius'

-- RESULT --
-- TODO add a custom color for result
result :: (Semigroup m, TrailLike (QDiagram b R2 m), IsName t) => t -> (QDiagram b R2 m, (t, Icon))
result name = (diagram, (name, Apply))
  where
    diagram = square 0.5 # fc (resC colorScheme) # lw none # named name

-- TEXT --
textInBox :: (Renderable (Path R2) b, IsName t) => String -> t -> (Diagram b R2, (t, Icon))
textInBox letters name = (diagram, (name, Text))
  where
    diagram = t # stroke # fc (textC colorScheme) # lw none <> (boundingRect t # lw none) # named name
    t = SVG.textSVG' $ SVG.TextOpts letters SVG.bit SVG.INSIDE_H SVG.KERN False 1 1

textInBox' :: Renderable (Path R2) b => String -> (Diagram b R2, (String, Icon))
textInBox' x = textInBox x x

-- GENERAL CONNECT FUNCTION --
data Icon = Apply | Text

conIcons :: (Enum t1, Enum t3, Renderable (Path R2) b, IsName t, IsName t1, IsName t2, IsName t3) => Colour Double -> (t, Icon) -> t1 -> (t2, Icon) -> t3 -> Diagram b R2 -> Diagram b R2
conIcons color (name1, icon1) port1 (name2, icon2) port2 =
  lc color. connectOutside' (lookupOptions icon1 port1 icon2 port2) (name1 .> port1) (name2 .> port2)
  
conPortIcon :: (Enum t1, Renderable (Path R2) b, IsName t, IsName t1, IsName n2) => Colour Double -> (t, Icon) -> t1 -> (n2, Icon) -> Diagram b R2 -> Diagram b R2
conPortIcon color (name1, icon1) port1 (name2, icon2) =
  lc color. connectOutside' (lookupOptions icon1 port1 icon2 (0::Integer)) (name1 .> port1) name2

conIconPort :: (Enum a2, Renderable (Path R2) b, IsName n1, IsName a1, IsName a2) => Colour Double -> (n1, Icon) -> (a1, Icon) -> a2 -> Diagram b R2 -> Diagram b R2
conIconPort color (name1, icon1) (name2, icon2) port2 =
  lc color . connectOutside' (lookupOptions icon1 0 icon2 port2) name1 (name2 .> port2)

conIconIcon :: (Renderable (Path R2) b, IsName n1, IsName n2) => Colour Double -> (n1, Icon) -> (n2, Icon) -> Diagram b R2 -> Diagram b R2
conIconIcon color (name1, icon1) (name2, icon2) =
  lc color . connectOutside' (lookupOptions icon1 0 icon2 0) name1 name2

lookupOptions :: (Enum a, Enum b) => Icon -> a -> Icon -> b -> ArrowOpts
lookupOptions icon1 port1 icon2 port2 = (with :: ArrowOpts) & setTailOpts icon1 port1 & setHeadOpts icon2 port2
  where
    setTailOpts Text _ = tailGap .~ gapSize
    setTailOpts Apply _ = id
    setHeadOpts Text _ = (arrowHead .~ noHead) . (headGap .~ gapSize)
    setHeadOpts Apply _ = arrowHead .~ noHead

-- MAIN --
main :: IO()
main = mainWith (test # frame 0.1)

test :: Diagram B R2
test = fst $ funApply argColor0 "test"
