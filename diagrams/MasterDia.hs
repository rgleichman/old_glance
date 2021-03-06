{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module MasterDia (main, masterDia, saveAll, saveMaster) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import DrawUtil
import Dia1 (d1)
import Dia1_1 (d1_1)
import Dia2 (d2)
import Dia3 (d3)
import Dia4 (d4)
import Dia4_1 (d4_1)
import Dia5 (d5)
import Dia6 (d6)
import Dia7 (d7)
import Diagrams.Backend.SVG (renderSVG)
import System.FilePath ((</>), addExtension)

-- For emacs: in image window set M-x auto-revert-mode
-- If reverts are slow, then M-x customize-variable auto-revert-interval

-- MAIN --
main :: IO()
main = mainWith (masterDia # frame 0.1)

saveMaster :: IO ()
saveMaster = renderSVG "master.svg" (Width 200) masterDia

diagramsInfo :: [(Diagram B R2, String, SizeSpec2D)]
diagramsInfo = [(d1,"d1", Height 150)
              ,(d1_1,"d1-1",Height 150)
              ,(d2,"d2",Height 150)
              ,(d3,"d3",Height 150)
              ,(d4,"d4",Height 150)
              ,(d4_1,"d4_1",Height 150)
              ,(d5,"d5", Height 150)
              ,(d6,"d6",Height 150)
              ,(d7,"d7",Height 200)]

diagrams :: [Diagram B R2]
diagrams = fmap (\(d,_,_)->d) diagramsInfo

--save all diagrams t
saveAll :: IO ()
saveAll = mapM_ saveDiagram diagramsInfo
  where
    saveDiagram (diagram, name, size) = renderSVG (addExtension (folder </> name) "svg") size framedDiagram
      where
        framedDiagram = diagram # frame 0.1 # bg backgroundColor
    folder = "svg"

masterDia :: Diagram B R2
masterDia = vcat'
            (with & sep .~ 0.5)
            diagrams
            # bg backgroundColor
