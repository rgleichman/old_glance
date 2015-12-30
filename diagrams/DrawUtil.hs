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
       ,funParamNoColor
       ,funApply
       ,result
       ,textInBox
       ,textInBox'
       ,conIcons
       ,conIconPort
       ,conPortIcon
       ,conIconIcon
       ,scope
       ,scopePortIcon
       ,Icon(..)
       )
       where

import Diagrams.Prelude
import Graphics.SVGFonts as SVG
import Data.Data (Typeable)
import Diagrams.Backend.SVG.CmdLine
import Data.Colour.SRGB (sRGB24)
import Data.Maybe (fromMaybe)
import Diagrams.TwoD.Polygons (polyPolarTrail)
import Control.Lens ((^.))

-- COLO(U)RS --
colorScheme :: (Floating a, Ord a) => ColorStyle a
colorScheme = colorOnBlackScheme
--colorScheme = blackColorScheme
--colorScheme = colorOnWhite

data ColorStyle a = ColorStyle {funC0 :: Colour a
                               ,argC0 :: Colour a
                               ,appTriC :: Colour a
                               ,outC :: Colour a
                               ,funAppFC :: Colour a
                               ,bgC :: Colour a
                               ,textC :: Colour a
                               ,resC :: Colour a
                               ,lamC :: Colour a
                               ,outlineC :: Maybe (Colour a)
                                }

colorOnWhite :: (Floating a, Ord a) => ColorStyle a
colorOnWhite =
  ColorStyle {funC0 = red
             ,argC0 = blue
             ,appTriC = lime
             ,outC = black
             ,funAppFC = yellow
             ,bgC = white
             ,textC = black
             ,resC = black
             ,lamC = purple
             ,outlineC = Just black}
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
             ,outlineC = Nothing}

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
             ,outlineC = Nothing}
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
applyTriangle color = argumentOrParam 1 applyTriColor (Just color)

funParam :: (Monoid m, Semigroup m, TrailLike (QDiagram b R2 m), IsName t) => Colour Double -> t -> (QDiagram b R2 m, (t, Icon))
funParam color = funParam' (Just color)

funParamNoColor :: (Monoid m, Semigroup m, TrailLike (QDiagram b R2 m), IsName t) => t -> (QDiagram b R2 m, (t, Icon))
funParamNoColor = funParam' Nothing

funParam' :: (Monoid m, Semigroup m, TrailLike (QDiagram b R2 m), IsName t) => Maybe (Colour Double) -> t -> (QDiagram b R2 m, (t, Icon))
funParam' = argumentOrParam 2.5 backgroundColor

argumentOrParam :: (Monoid m, Semigroup m, TrailLike (QDiagram b R2 m), IsName t) => Double -> Colour Double -> Maybe (Colour Double) -> t -> (QDiagram b R2 m, (t, Icon))
argumentOrParam outlineScale insideColor mColor name = (name |> diagram, (name, Apply))
  where
    diagram = line
              <> baseTri # fc insideColor # lc outlineColor # lwL outlineWidth # named name
              <> decoratePath (scale 0.9 baseTri) (map (\x -> mempty # named x) [AppR, AppOut, AppL]) <> mempty # named AppIn
    baseTri = triangle size # rotateBy (1/4) # alignR
    size = 0.7
    outlineWidth = outlineScale*outlineScaleFactor*size :: Double
    squarRootThree = 1.732
    outlineColor =
      fromMaybe applyTriColor $ outlineC colorScheme
    line = case mColor of
      Nothing -> mempty
      Just color -> vrule (size + (outlineWidth * squarRootThree)) # lwL defLineWidth # lc color

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
    outlineColor = fromMaybe funApplyFillColor $ outlineC colorScheme
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

-- CONNECT FUNCTIONS --
data Icon = Apply | Text

generalConnect :: (Renderable (Path R2) b, IsName a, IsName b1, IsName a1, IsName b2) => (ArrowOpts -> ArrowOpts) -> Colour Double -> (a, Icon) -> Maybe b1 -> (a1, Icon) -> Maybe b2 -> Diagram b R2 -> Diagram b R2
generalConnect customOptions color (name1, icon1) port1 (name2, icon2) port2 =
  lc color
  .
  connectOutside'
  options
  (qualName name1 port1)
  (qualName name2 port2)
  where
    options = lookupOptions icon1 port1 icon2 port2
              & customOptions
    qualName :: (IsName a, IsName b) => a -> Maybe b -> Name
    qualName name port =
      case port of
        Nothing -> toName name
        Just p -> name .> p

conIcons :: (Renderable (Path R2) b, IsName a, IsName b1, IsName a1, IsName b2) => Colour Double -> (a, Icon) -> b1 -> (a1, Icon) -> b2 -> Diagram b R2 -> Diagram b R2
conIcons color n1 p1 n2 p2= conIcons' color n1 (Just p1) n2 (Just p2)

conIcons' :: (Renderable (Path R2) b, IsName a, IsName b1, IsName a1, IsName b2) => Colour Double -> (a, Icon) -> Maybe b1 -> (a1, Icon) -> Maybe b2 -> Diagram b R2 -> Diagram b R2
conIcons' = generalConnect $ arrowHead .~ noHead

conPortIcon :: (Renderable (Path R2) b, IsName a, IsName b1, IsName a1) => Colour Double -> (a, Icon) -> b1 -> (a1, Icon) -> Diagram b R2 -> Diagram b R2
conPortIcon color n1 port1 n2 =
  conIcons' color n1 (Just port1) n2 (Nothing :: Maybe Int)

conIconPort :: (Renderable (Path R2) b, IsName a, IsName a1, IsName b2) => Colour Double -> (a, Icon) -> (a1, Icon) -> b2 -> Diagram b R2 -> Diagram b R2
conIconPort color n1 n2 port2 =
  conIcons' color n1 (Nothing :: Maybe Int) n2 (Just port2)

conIconIcon :: (Renderable (Path R2) b, IsName a, IsName a1) => Colour Double -> (a, Icon) -> (a1, Icon) -> Diagram b R2 -> Diagram b R2
conIconIcon color n1 n2 =
  conIcons' color n1 (Nothing :: Maybe Int) n2 (Nothing :: Maybe Int)

-- TODO possible add an enum restriction to the ports
--lookupOptions :: (Enum a, Enum b) => Icon -> a -> Icon -> b -> ArrowOpts
lookupOptions :: Icon -> t -> Icon -> t1 -> ArrowOpts
lookupOptions icon1 port1 icon2 port2 = (with :: ArrowOpts) & setTailOpts icon1 port1 & setHeadOpts icon2 port2
  where
    setTailOpts Text _ = tailGap .~ gapSize
    setTailOpts Apply _ = id
    setHeadOpts Text _ = headGap .~ gapSize
    setHeadOpts Apply _ = id

-- SCOPE --

--TODO: use a more distinc arrow head
scope :: (Renderable (Path R2) b, IsName a, IsName b1, IsName a1, IsName b2) => (a, Icon) -> Maybe b1 -> (a1, Icon) -> Maybe b2 -> Diagram b R2 -> Diagram b R2
scope = generalConnect style (textC colorScheme)
  where style = (shaftStyle %~ dashingG [0.07, 0.07] 0)
                . (arrowHead .~ spike)
                . (headLength .~ Local 0.5)
                . (headTexture .~ solid (lamC colorScheme))

scopePortIcon :: (Renderable (Path R2) b, IsName a, IsName b1, IsName a1) => (a, Icon) -> b1 -> (a1, Icon) -> Diagram b R2 -> Diagram b R2
scopePortIcon n1 p1 n2 = scope n1 (Just p1) n2 (Nothing :: Maybe Int)

-- IF ELSE --

--ifElse :: (Semigroup a, TrailLike a, Transformable a, V a ~ R2) => Angle -> a
ifElse :: Angle -> Diagram B R2
ifElse outAngle = (circle 1 <> outputDiagram)
  where
    output :: Located (Trail' Line R2)
    output = fromVertices (map polarP2 [(1, outAngleTurn - nodeAngle @@ turn),(1.3, outAngle),(1, outAngleTurn + nodeAngle @@ turn)])
    outputDiagram = closeLineLocated output
    --outputDiagram = output # closeLine # strokeLoop # translate (r2 $ unp2 $ p1)
    input = fromVertices
    p1 = polarP2 (1, outAngleTurn - nodeAngle @@ turn)
    --output = polyPolarTrail [0.1 @@ turn, 0.3 @@ turn] [0.5,2] # trailLike
    outAngleTurn = outAngle ^. turn
    nodeAngle = (1/24) -- turn
    
closeLineLocated :: Renderable (Path R2) b => Located (Trail' Line R2) -> Diagram b R2
closeLineLocated trail = unLocated # translate (r2 $ unp2 $ loc trail)
  where unLocated = trail # unLoc # closeLine # strokeLoop

polarP2 :: (Double, Angle) -> P2
polarP2 (0,_) = p2 (0,0)
polarP2 (r, theta) = rotate theta . scale r $ p2 (1,0)

-- MAIN --
main :: IO()
main = mainWith (ifElseTest # frame 0.1)

test :: Diagram B R2
test = fst $ funApply argColor0 "test"

--ifElseTest :: Diagram B R2
ifElseTest :: Diagram B R2
ifElseTest = ifElse ((1/3) @@ turn)
