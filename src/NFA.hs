module NFA
  ( NFA(..)
  , orbitToText
  ) where

import qualified Data.Graph.Inductive as Gr
import           Data.GraphViz
import qualified Data.Set             as Set
import qualified Data.Text            as T

orbitToText :: (Show state) => Set.Set state -> T.Text
orbitToText o =
  mconcat ["{", T.intercalate "," $ map (T.pack . show) $ Set.toList o, "}"]

class NFA nfa where
  type StateType nfa :: *
  type TransitionType nfa :: *
  emptyNFA :: nfa
  addState :: StateType nfa -> nfa -> Maybe nfa
  addTransition ::
       (StateType nfa, StateType nfa, TransitionType nfa) -> nfa -> Maybe nfa
  stateExist :: StateType nfa -> nfa -> Bool
  getStates :: nfa -> [StateType nfa]
  isFinal :: StateType nfa -> nfa -> Bool
  isStart :: StateType nfa -> nfa -> Bool
  initialStates :: nfa -> Set.Set (StateType nfa)
  finalStates :: nfa -> Set.Set (StateType nfa)
  transitionExist ::
       (StateType nfa, StateType nfa, TransitionType nfa) -> nfa -> Bool
  hasEdge :: (StateType nfa, StateType nfa) -> nfa -> Bool
  removeState :: StateType nfa -> nfa -> Maybe nfa
  removeTransition ::
       (StateType nfa, StateType nfa, TransitionType nfa) -> nfa -> Maybe nfa
  removeTransitions :: (StateType nfa, StateType nfa) -> nfa -> Maybe nfa
  makeInit :: StateType nfa -> nfa -> Maybe nfa
  makeFinal :: StateType nfa -> nfa -> Maybe nfa
  isStandard :: nfa -> Bool
  isHomogeneous :: nfa -> Bool
  makeStandard :: (Enum (StateType nfa)) => nfa -> nfa
--   makeHomogeneous ::Enum (StateType nfa) => nfa -> nfa
  directSucc :: StateType nfa -> nfa -> Set.Set (StateType nfa)
  directPred :: StateType nfa -> nfa -> Set.Set (StateType nfa)
  extractListStateAutomata :: Set.Set (StateType nfa) -> nfa -> Maybe nfa
  maximalOrbits :: nfa -> [Set.Set (StateType nfa)]
  isOrbit :: Set.Set (StateType nfa) -> nfa -> Bool
  orbitIn :: Set.Set (StateType nfa) -> nfa -> Set.Set (StateType nfa)
  orbitOut :: Set.Set (StateType nfa) -> nfa -> Set.Set (StateType nfa)
  isStableOrbit :: Set.Set (StateType nfa) -> nfa -> Bool
  isStronglyStableOrbit :: Set.Set (StateType nfa) -> nfa -> Bool
  isTransversOrbit :: Set.Set (StateType nfa) -> nfa -> Bool
  isStronglyTransversOrbit :: Set.Set (StateType nfa) -> nfa -> Bool
  accept :: [TransitionType nfa] -> nfa -> Bool
  automatonToDot ::
       (Show (StateType nfa), Show (TransitionType nfa))
    => nfa
    -> DotGraph Gr.Node
  automatonToDotClustered ::
       (Show (StateType nfa), Show (TransitionType nfa))
    => [Set.Set (StateType nfa)]
    -> nfa
    -> DotGraph Gr.Node
