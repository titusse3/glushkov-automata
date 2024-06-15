module Prop.PropNFA
  ( AG(..)
  , AF(..)
  , propAddState
  , propAddTransition
  , propRemoveState
  , propRemoveTransition
  , propRemoveTransitions
  , propDirectSucc
  , propDirectPred
  , propStandard
  -- , propHomogenous
  ) where

import           NFA

import qualified NFAF            as NF
import qualified NFAG            as NG
import           Test.QuickCheck

newtype AG state transition = AG
  { ag :: NG.NFAG state transition
  } deriving (Show)

newtype AF state transition = AF
  { af :: NF.NFAF state transition
  } deriving (Show)

instance Arbitrary (AG Int Char) where
  arbitrary = do
    states <- listOf arbitrary
    transitions <- listOf arbitrary
    return
      $ foldl
          (\a t -> maybe a AG (addTransition t (ag a)))
          (aState states)
          transitions
    where
      aState =
        foldl (\a n -> maybe a AG (addState n (ag a)))
          $ AG (emptyNFA :: NG.NFAG Int Char)

instance Arbitrary (AF Int Char) where
  arbitrary = do
    states <- listOf arbitrary
    transitions <- listOf arbitrary
    return
      $ foldl
          (\a t -> maybe a AF (addTransition t (af a)))
          (aState states)
          transitions
    where
      aState =
        foldl (\a n -> maybe a AF (addState n (af a)))
          $ AF (emptyNFA :: NF.NFAF Int Char)

propAddState :: NFA nfa => StateType nfa -> nfa -> Bool
propAddState state a =
  case addState state a of
    Just a' -> stateExist state a'
    Nothing -> stateExist state a

propAddTransition ::
     NFA nfa
  => (StateType nfa, StateType nfa, TransitionType nfa)
  -> nfa
  -> Bool
propAddTransition (q0, q1, c) a =
  case addTransition (q0, q1, c) a of
    Just a' -> transitionExist (q0, q1, c) a'
    Nothing ->
      not (stateExist q0 a)
        || not (stateExist q1 a)
        || transitionExist (q0, q1, c) a

propRemoveState :: NFA nfa => StateType nfa -> nfa -> Bool
propRemoveState state a =
  case removeState state a of
    Just a' -> not (stateExist state a')
    Nothing -> not (stateExist state a)

propRemoveTransition ::
     NFA nfa
  => (StateType nfa, StateType nfa, TransitionType nfa)
  -> nfa
  -> Bool
propRemoveTransition (q0, q1, c) a =
  case removeTransition (q0, q1, c) a of
    Just a' -> not (transitionExist (q0, q1, c) a')
    Nothing ->
      not (stateExist q0 a)
        || not (stateExist q1 a)
        || not (transitionExist (q0, q1, c) a)

propRemoveTransitions ::
     NFA nfa => (StateType nfa, StateType nfa) -> nfa -> Bool
propRemoveTransitions couple a =
  case removeTransitions couple a of
    Just a' -> not (hasEdge couple a')
    Nothing -> not (hasEdge couple a)

propDirectSucc :: NFA nfa => StateType nfa -> nfa -> Bool
propDirectSucc state a =
  all (\s -> hasEdge (state, s) a) (directSucc state a)

propDirectPred :: NFA nfa => StateType nfa -> nfa -> Bool
propDirectPred state a =
  all (\s -> hasEdge (s, state) a) (directPred state a)

propStandard :: (NFA nfa, Enum (StateType nfa)) => nfa -> Bool
propStandard = isStandard . makeStandard

-- propHomogenous :: (NFA nfa, Enum (StateType nfa)) => nfa -> Bool
-- propHomogenous = isHomogeneous . makeHomogeneous