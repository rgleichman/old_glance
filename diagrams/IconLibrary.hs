module IconLibrary where
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import DrawUtil

t0 :: Diagram B R2
t0Name :: (String, Icon)
--(t0, t0Name) = applyTriangle argColor0 "t0"
(t0, t0Name) = applyTriangle argColor0 "t0"

t1 :: Diagram B R2
t1Name :: (String, Icon)
(t1, t1Name) = applyTriangle argColor0 "t1"

pRaw :: String -> (Diagram B R2, (String, Icon))
pRaw = funParam argColor0

p0 :: Diagram B R2
p0Name :: (String, Icon)
(p0, p0Name) = funParam argColor0 "p0"

p1 :: Diagram B R2
p1Name :: (String, Icon)
(p1, p1Name) = funParam argColor0 "p1"

fa0 :: Diagram B R2
fa0Name :: (String, Icon)
(fa0, fa0Name) = funApply funColor0 "fa0"

fa1 :: Diagram B R2
fa1Name :: (String, Icon)
(fa1, fa1Name) = funApply funColor0 "fa1"

f0 :: Diagram B R2
f0Name :: (String, Icon)
(f0, f0Name) = textInBox "f" "f0"

f1 :: Diagram B R2
f1Name :: (String, Icon)
(f1, f1Name) = textInBox' "f1"

f2 :: Diagram B R2
f2Name :: (String, Icon)
(f2, f2Name) = textInBox' "f2"

yRaw :: String -> (Diagram B R2, (String, Icon))
yRaw = textInBox "y"

y0 :: Diagram B R2
y0Name :: (String, Icon)
(y0, y0Name) = textInBox "y" "y0"

y1 :: Diagram B R2
y1Name :: (String, Icon)
(y1, y1Name) = textInBox' "y1"

y2 :: Diagram B R2
y2Name :: (String, Icon)
(y2, y2Name) = textInBox' "y2"

x0 :: Diagram B R2
x0Name :: (String, Icon)
(x0, x0Name) = textInBox "x" "x0"

x1 :: Diagram B R2
x1Name :: (String, Icon)
(x1, x1Name) = textInBox' "x1"

x2 :: Diagram B R2
x2Name :: (String, Icon)
(x2, x2Name) = textInBox' "x2"
