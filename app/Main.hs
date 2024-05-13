module Main
  ( main
  ) where

import           Data.GraphViz          (printDotGraph)
import           Data.Maybe             (fromJust)
import qualified Data.Text.Lazy.IO      as TIO
import qualified Data.Text      as T
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
  -- let t = "(a+b)*"
  -- let t = "$"
  -- t <- TIO.getLine
  -- rajouter un module
  let expT = fromJust $ H.parseExp (A.alexScanTokens t)
  let gluskov = glushkov expT
  let gluskov' = N.addTransition gluskov (0, 2, 'a')
  print $ N.isHomogeneous gluskov'
  -- Sans cluster
  -- TIO.putStr $ printDotGraph $ N.automatonToDotClustered gluskov
  -- -- Avec cluster
  -- TIO.putStr $ printDotGraph $ N.automatonToDot gluskov
  void
    $ addExtension
        (runGraphviz (N.automatonToDotClustered gluskov'))
        Png
        "testPNG"
