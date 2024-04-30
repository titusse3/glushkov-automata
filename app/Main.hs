module Main
  ( main
  ) where

import           Data.GraphViz     (printDotGraph)
import           Data.Maybe        (fromJust)
import qualified Data.Text.Lazy.IO as TIO
import           Exp               (glushkov)
import qualified ExpHp             as H
import qualified ExpLex            as A
import qualified NFA               as N

main :: IO ()
main = do
  let t = "(a+b).a*.b*.(a+b)*"
  -- let t = "(a+b)*"
  -- let t = "$"
  -- t <- TIO.getLine
  let expT = fromJust $ H.parseExp (A.alexScanTokens t)
  let gluskov = glushkov expT
  -- Sans cluster
  TIO.putStr $ printDotGraph $ N.automatonToDotClustered gluskov
  -- Avec cluster
  TIO.putStr $ printDotGraph $ N.automatonToDot gluskov
