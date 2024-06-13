module NFA
  ( NFA(..)
  , orbitToText
  ) where

import qualified Data.Set                          as Set
import qualified Data.Text as T
import qualified Data.Graph.Inductive              as Gr
import           Data.GraphViz

orbitToText :: (Show state) => (Set.Set state) -> T.Text
orbitToText o =
  mconcat ["{", (T.intercalate "," $ map (T.pack . show) $ Set.toList o), "}"]

class (Ord state, Ord transition) => NFA state transition where
  emptyNFA :: nfa state transition

  addState :: state -> nfa state transition -> Maybe (nfa state transition)

  addTransition :: (state, state, transition) -> nfa state transition -> Maybe (nfa state transition)

  stateExist :: state -> nfa state transition -> Bool

  isFinal :: state -> nfa state transition -> Bool

  isStart :: state -> nfa state transition -> Bool

  transitionExist :: (state, state, transition) -> nfa state transition -> Bool

  hasEdge :: (state, state) -> nfa state transition -> Bool

  removeState :: state -> nfa state transition -> Maybe (nfa state transition)

  removeTransition :: (state, state, transition) -> nfa state transition -> Maybe (nfa state transition)

  removeTransitions :: (state, state) -> nfa state transition -> Maybe (nfa state transition)

  makeInit :: state -> nfa state transition -> Maybe (nfa state transition)

  makeFinal :: state -> nfa state transition -> Maybe (nfa state transition)

  isStandard :: nfa state transition -> Bool

  isHomogeneous :: nfa state transition -> Bool

  makeStandard :: nfa state transition -> nfa state transition

  directSucc :: state -> nfa state transition -> Set.Set state

  directPred :: state -> nfa state transition -> Set.Set state

  extractListStateAutomata :: Set.Set state -> nfa state transition -> Maybe (nfa state transition)

  maximalOrbits :: nfa state transition -> [Set.Set state]

  isOrbit :: Set.Set state -> nfa state transition -> Bool

  orbitIn :: Set.Set state -> nfa state transition -> Set.Set state

  orbitOut :: Set.Set state -> nfa state transition -> Set.Set state

  isStableOrbit :: Set.Set state -> nfa state transition -> Bool

  isStronglyStableOrbit :: Set.Set state -> nfa state transition -> Bool

  isTransversOrbit :: Set.Set state -> nfa state transition -> Bool

  isStronglyTransversOrbit :: Set.Set state -> nfa state transition -> Bool

  accept :: [transition] -> nfa state transition -> Bool

  automatonToDot :: (Show state, Show transition) => nfa state transition -> DotGraph Gr.Node

  automatonToDotClustered :: (Show state, Show transition) => [Set.Set state] -> nfa state transition -> DotGraph Gr.Node
  
