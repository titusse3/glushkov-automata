module Main (main) where

import Control.Monad (join)
import qualified ExpLex as A
import qualified ExpHp as H
import Exp ( followE, linearisation, alphabet, firstE, lastE, glushkov )
import NFA

import qualified Data.Set as Set

import Data.Maybe ( fromJust )

-- main :: IO ()
-- main = do
--   let t = "(a+b).a*.b*.(a+b)*"
--   -- t <- TIO.getLine
--   let expT = fromJust $ H.parseExp (A.alexScanTokens t)
--   let lineE = linearisation expT
--   let f = followE lineE 
--   let symbols = alphabet expT
--   let gg = glushkov expT
--   let trans = delta gg
--   let positions = etats gg
--   putStrLn $ displayNFA gg

import           Data.Graph.Inductive
import           Data.GraphViz
import Data.GraphViz.Printing
import           Data.GraphViz.Attributes.Complete
import qualified Data.Text.Lazy.IO as TIO
import  qualified         Data.Text.Lazy                    as L

-- Définition de votre graphe
-- exGraph :: Gr Text Text
ex1 :: Gr L.Text L.Text
ex1 = mkGraph [ (1,"one")
              , (3,"three")
              ]
              [ (1,3,"edge label") ]

ex1Params :: GraphvizParams n L.Text L.Text () L.Text
ex1Params = nonClusteredParams { globalAttributes = ga
                               , fmtNode          = fn
                               , fmtEdge          = fe
                               }
  where fn (_,l)   = [textLabel l]
        fe (_,_,l) = [textLabel l]

        ga = [ GraphAttrs [ RankDir   FromLeft
                          ]
             , NodeAttrs  [ shape      DoubleCircle --shape     DoubleCircle
                          ]
             ]

-- Fonction principale pour transformer le graphe en DOT et l'afficher
main :: IO ()
main = do
  -- let params = nonClusteredParams { fmtNode = \(_, l) -> [Label $ toLabelValue l]
  --                                 , fmtEdge = \(_, _, l) -> [Label $ toLabelValue l] }
  TIO.putStrLn $ renderDot $ toDot $ graphToDot ex1Params ex1
