module Main
  ( main
  ) where

import           Data.GraphViz          (printDotGraph)
import           Data.Maybe             (fromJust)
import qualified Data.Text.Lazy.IO      as TIO
-- import qualified Data.Text      as T
import           Exp                    (glushkov)
import qualified ExpHp                  as H
import qualified ExpLex                 as A
import qualified NFA                    as N

import           Control.Monad          (void)
import           Data.GraphViz.Commands (GraphvizOutput (Png), addExtension,
                                         runGraphviz)

main :: IO ()
main = do
  let t = "(a+b).a*.b*.(a+b)*"
  let expT = fromJust $ H.parseExp (A.alexScanTokens t)
  let gluskov = glushkov expT
  let gluskov' = N.removeState (N.removeState (N.addTransition gluskov (0, 2, 'a')) 1) 3
  print $ N.isHomogeneous gluskov'
  print $ N.etats gluskov'
  -- Sans cluster
  -- TIO.putStr $ printDotGraph $ N.automatonToDotClustered gluskov
  TIO.putStr $ printDotGraph $ N.automatonToDot gluskov'
  -- -- Avec cluster
  -- TIO.putStr $ printDotGraph $ N.automatonToDot gluskov
  void
    $ addExtension
        (runGraphviz (N.automatonToDotClustered gluskov'))
        Png
        "testPNG"
