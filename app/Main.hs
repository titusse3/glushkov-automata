module Main
  ( main
  ) where

import           Control.Monad                     (join)
import           Exp                               (alphabet, firstE, followE,
                                                    glushkov, lastE,
                                                    linearisation)
import qualified ExpHp                             as H
import qualified ExpLex                            as A
import           NFA                               (automatonToDot)

import qualified Data.Set                          as Set

import           Data.Maybe                        (fromJust)

import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree
import           Data.GraphViz
import           Data.GraphViz.Attributes.Complete
import           Data.Text.Internal.Lazy           as LTIO
import qualified Data.Text.Lazy                    as L
import qualified Data.Text.Lazy.IO                 as TIO

main :: IO ()
main = do
  let t = "(a+b).a*.b*.(a+b)*"
  -- t <- TIO.getLine
  let expT = fromJust $ H.parseExp (A.alexScanTokens t)
  let gluskov = glushkov expT
  let c = automatonToDot gluskov
  TIO.putStr $ printDotGraph c
