module Main
  ( main
  ) where

import           Data.GraphViz     (printDotGraph)
import           Data.Maybe        (fromJust)
import qualified Data.Text.Lazy.IO as TIO
import           Exp               (glushkov)
import qualified ExpHp             as H
import qualified ExpLex            as A
import           NFA               (accept, automatonToDot, automataToGraph)
import Data.Graph.Inductive.Query.DFS

main :: IO ()
main = do
  -- let t = "(a+b).a*.b*.(a+b)*"
  let t = "(a+b)*"
  -- t <- TIO.getLine
  let expT = fromJust $ H.parseExp (A.alexScanTokens t)
  let gluskov = glushkov expT
  TIO.putStr $ printDotGraph $ automatonToDot gluskov
  print $ accept gluskov "ab"
  let sccs = scc $ automataToGraph gluskov
  putStrLn $ "// Composantes fortement connexes : "
  mapM_ print sccs