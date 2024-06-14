import           Criterion.Main
import           Criterion.Types (Config (..), Verbosity (..))

import qualified NFA             as N
import qualified NFAF            as NF
import qualified NFAG            as NG

import           Control.Monad   (foldM)
import           Data.Maybe      (fromJust)
import           System.Random   (randomRIO)

creatAutomataNState :: (N.NFA nfa, N.StateType nfa ~ Int) => Int -> nfa
creatAutomataNState n =
  foldl (\a x -> fromJust $ N.addState x a) N.emptyNFA [1 .. n]

addTransitions ::
     (N.NFA nfa, N.StateType nfa ~ Int, N.TransitionType nfa ~ Int)
  => nfa
  -> nfa
addTransitions nfa =
  foldl
    (\a q0 -> foldl (\a' q1 -> fromJust $ N.addTransition (q0, q1, 0) a') a l)
    nfa
    l
  where
    l = N.getStates nfa

removeAllTransitions :: (N.NFA nfa, N.StateType nfa ~ Int) => nfa -> Maybe nfa
removeAllTransitions nfa =
  foldM (\a q0 -> foldM (\a' q1 -> N.removeTransitions (q0, q1) a') a l) nfa l
  where
    l = N.getStates nfa

randomAutomata ::
     ( N.NFA nfa
     , N.StateType nfa ~ Int
     , N.TransitionType nfa ~ Int
     , N.NFA nfa'
     , N.StateType nfa' ~ Int
     , N.TransitionType nfa' ~ Int
     )
  => Int
  -> Int
  -> IO (nfa, nfa')
randomAutomata nState nTrans = foldM addRandomTrans (a, a') [1 .. nTrans]
  where
    a = creatAutomataNState nState
    a' = creatAutomataNState nState
    l = N.getStates a
    addRandomTrans (a1, a2) _ = do
      q0 <- randomRIO (0, nState - 1)
      q1 <- randomRIO (0, nState - 1)
      let q0' = l !! q0
          q1' = l !! q1
          a1' = fromJust $ N.addTransition (q0', q1', 0) a1
          a2' = fromJust $ N.addTransition (q0', q1', 0) a2
      return (a1', a2')

main :: IO ()
main = do
  let config =
        defaultConfig {reportFile = Just "report.html", verbosity = Verbose}
      n = 1000
      aNF = creatAutomataNState n :: NF.NFAF Int Int
      aNG = creatAutomataNState n :: NG.NFAG Int Int
  (aRandG, aRandF) <-
    randomAutomata 2000 2000 :: IO (NG.NFAG Int Int, NF.NFAF Int Int)
  let stabilityChecks a = map (`N.isStronglyStableOrbit` a) $ N.maximalOrbits a
  let transverseChecks a =
        map (`N.isStronglyTransversOrbit` a) $ N.maximalOrbits a
  defaultMainWith
    config
    [ bgroup
        "Automaton Operations"
        [ bgroup
            "NF"
            [ bench ("addState " ++ show n ++ " (NF)")
                $ whnf (\n' -> creatAutomataNState n' :: NF.NFAF Int Int) n
            , bench ("addTransitions " ++ show n ++ " (NF)")
                $ whnf addTransitions aNF
            , bench ("removeTransitions " ++ show n ++ " (NF)")
                $ whnf removeAllTransitions aNF
            , bench "stabilityChecks (NF)" $ whnf stabilityChecks aRandF
            , bench "transverseChecks (NF)" $ whnf transverseChecks aRandF
            ]
        , bgroup
            "NG"
            [ bench ("addState " ++ show n ++ " (NG)")
                $ whnf (\n' -> creatAutomataNState n' :: NG.NFAG Int Int) n
            , bench ("addTransitions " ++ show n ++ " (NG)")
                $ whnf addTransitions aNG
            , bench ("removeTransitions " ++ show n ++ " (NG)")
                $ whnf removeAllTransitions aNG
            , bench "stabilityChecks (NG)" $ whnf stabilityChecks aRandG
            , bench "transverseChecks (NG)" $ whnf transverseChecks aRandG
            ]
        ]
    ]
