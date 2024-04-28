module NFA(NFA(..), automataToGraph) where

import qualified Data.Set as Set

import Data.Maybe ( fromMaybe )
import Data.Graph.Inductive ( Gr, Graph(mkGraph) )

-- import Data.GraphViz
-- import Data.GraphViz.Attributes.Complete

data NFA state transition = NFA {
  sigma :: Set.Set transition,
  etats :: Set.Set state,
  premier :: Set.Set state,
  final :: Set.Set state,
  delta :: state -> transition -> Set.Set state
}

automataToGraph :: (Ord state, Show state, Show transition) => 
  NFA state transition -> Gr String String
automataToGraph a = mkGraph nodes edges
  where
    nodes = zip indices (map show states)
    edges = [(stateIndex s, stateIndex t, formatString $ show tr) | 
              (s, trs) <- stateTrans, tr <- Set.toList trs,
              t <- Set.toList (deltaFun s tr)]
    states = Set.toList $ etats a
    indices = [1..]
    stateIndex state = fromMaybe (-1) (lookup state $ zip states indices)
    stateTrans = [(s, sigma a) | s <- states] 
    deltaFun = delta a
    formatString = filter (/= '\'')