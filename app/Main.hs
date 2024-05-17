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
  jsonStr <- B.readFile "automaton2.json"
  let z = JNFA.parseNFA jsonStr :: Maybe (N.NFA Int T.Text)
  let z' = fromJust z
  let o = head $ N.maximalOrbit z'
  print $ N.isStronglyOrbit z' o
  -- void
  --   $ addExtension
  --       (runGraphviz (N.automatonToDotClustered a ))
  --       Png
  --       "testPNG"
