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

import qualified Data.Text              as T

main :: IO ()
main = do
  let t = "(a+b).a*.b*.(a+b)*"
  let expT = fromJust $ H.parseExp (A.alexScanTokens t)
  let gluskov = glushkov expT
  let gluskov' = N.removeState gluskov 1
  TIO.putStr $ printDotGraph $ N.automatonToDot gluskov'
  print $ N.maximalOrbit gluskov'
  print $ N.orbitOut gluskov' $ head $ N.maximalOrbit gluskov'
  let jsonStr =
        "{\"Node\": [0, 1, 2, 3, 4], \"First\": [0, 2], \"Final\": [1], \"Transition\": [[0, 1, \"a\"], [4, 3, \"b\"]]}" :: B.ByteString
  let z = JNFA.parseNFA jsonStr :: Maybe (N.NFA Int T.Text)
  let z' = fromJust z
  void $ addExtension (runGraphviz (N.automatonToDotClustered z')) Png "testPNG"
