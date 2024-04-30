module NFA
  ( NFA(..)
  , automataToGraph
  , isFinal
  , isStart
  , automatonToDot
  , accept
  ) where

import           Data.Graph.Inductive
import Data.Graph.Inductive.Query.DFS

import           Data.GraphViz                     
import           Data.GraphViz.Attributes.Complete
import           Data.Maybe                        (fromMaybe)
import qualified Data.Set                          as Set
import qualified Data.Text                         as T
import qualified Data.Text.Lazy                    as LT

data NFA state transition = NFA
  { sigma   :: Set.Set transition
  , etats   :: Set.Set state
  , premier :: Set.Set state
  , final   :: Set.Set state
  , delta   :: state -> transition -> Set.Set state
  }

isFinal :: Ord state => NFA state transition -> state -> Bool
isFinal a = flip Set.member (final a)

isStart :: Ord state => NFA state transition -> state -> Bool
isStart a = flip Set.member (premier a)

accept :: Ord state => NFA state transition -> [transition] -> Bool
accept a = accept' i
  where
    d = delta a
    i = premier a
    f = final a
        -- accept' :: Set.Set state -> [transition] -> Bool
    accept' s [] = not $ Set.null $ Set.intersection s f
    accept' s (t:ts) =
      if Set.null s
        then False
        else accept' (Set.foldl (\s' x -> Set.union s' $ d x t) Set.empty s) ts

automataToGraph ::
     (Ord state, Show state, Show transition)
  => NFA state transition
  -> Gr T.Text T.Text
automataToGraph a = mkGraph nodesList edgesList
  where
    nodesList = zip indices $ map (T.pack . show) states
    edgesList =
      [ (stateIndex s, stateIndex t, formatText $ T.pack $ show tr)
      | (s, trs) <- stateTrans
      , tr <- Set.toList trs
      , t <- Set.toList (deltaFun s tr)
      ]
    states = Set.toList $ etats a
    indices = [0 ..]
    stateIndex state = fromMaybe (-1) (lookup state $ zip states indices)
    stateTrans = [(s, sigma a) | s <- states]
    deltaFun = delta a
    formatText = T.filter (/= '\'')

automatonToDot :: Show transition => NFA Node transition -> DotGraph Node
automatonToDot a = graphToDot params graph
  where
    graph = automataToGraph a
    params =
      nonClusteredParams
        { globalAttributes = [GraphAttrs [RankDir FromLeft]]
        , fmtNode =
            \node ->
              [ Shape $ shapeOf node
              , FillColor [toWColor $ colorOf node]
              , Style [SItem Filled []]
              , ExtraAttrs [("style", if isComponentNode node then "rounded,filled" else "filled")]
              ]
        , fmtEdge = \(_, _, l) -> [Label $ StrLabel $ LT.fromStrict l]
        }
    shapeOf (val, _)
      | isFinal a val = DoubleCircle
      | otherwise = Circle
    colorOf (val, _)
      | isStart a val = Green
      | otherwise = White
    isComponentNode node = any (elem node) scc
