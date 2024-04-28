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

-- import           Data.Graph.Inductive
-- import           Data.GraphViz
-- import           Data.GraphViz.Printing
-- import           Data.GraphViz.Attributes.Complete
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text.Lazy                    as L
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

-- Définition d'un type pour les états, les labels des transitions et si l'état est final
type AutomatonNode = (String, Bool)  -- (Nom de l'état, Est-ce un état final?)

-- Création d'un automate exemple
automaton :: Gr AutomatonNode String
automaton = mkGraph nodes edges
  where
    nodes = [(1, ("S0", False)), (2, ("S1", True)), (3, ("S2", False))]
    edges = [(1, 2, "a"), (2, 3, "b"), (1, 3, "c")]

-- Fonction pour définir le style des nœuds
nodeAttributes' :: AutomatonNode -> Attributes
nodeAttributes' (_, isFinal) = 
  [ shape $ if isFinal then DoubleCircle else Circle
  -- , style $ if isFinal then shape DoubleCircle else shape Circle
  ]

-- Fonction pour convertir l'automate en graph DOT
automatonToDot :: Gr AutomatonNode String -> DotGraph Node
automatonToDot aut = graphToDot params aut
  where
    params = nonClusteredParams { globalAttributes = [GraphAttrs [RankDir FromLeft]]
                                , fmtNode = \(_, label) -> nodeAttributes' label
                                , fmtEdge = \(_, _, l) -> [Label $ StrLabel $ L.pack l]
                                }

main :: IO ()
main = TIO.putStr $ printDotGraph $ automatonToDot automaton

