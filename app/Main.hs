module Main
  ( main
  ) where

import           Data.GraphViz          (printDotGraph)
import           Data.Maybe             (fromJust)
import qualified Data.Text.Lazy.IO      as TIO

import qualified Data.ByteString.Lazy   as B
import           Exp                    (glushkov)
import qualified ExpHp                  as H
import qualified ExpLex                 as A
import qualified JsonToNFA              as JNFA
import qualified NFA                    as N

import           Control.Monad          (void)
import           Data.GraphViz.Commands (GraphvizOutput (Png), addExtension,
                                         runGraphviz)

import qualified Data.Set               as Set
import qualified Data.Text              as T

main :: IO ()
main = do
  -- let t = "(a+b).a*.b*.(a+b)*"
  -- let expT = fromJust $ H.parseExp (A.alexScanTokens t)
  -- let gluskov = glushkov expT
  jsonStr <- B.readFile "test.json"
  let z = JNFA.parseNFA jsonStr :: Maybe (N.NFA Int T.Text)
  let z' = fromJust z
  let inO =  N.orbitIn z' $ Set.fromList [5, 6, 7]
  let outO =  N.orbitOut z' $ Set.fromList [5, 6, 7]
  print $ do 
    x <- Set.toList outO
    y <- Set.toList inO
    return (x, y)
  print $ N.maximalOrbit z'
  void $ addExtension (runGraphviz (N.automatonToDotClustered z')) Png "testPNG"
