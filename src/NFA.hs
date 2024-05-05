module NFA
  ( NFA(..)
  , automataToGraph
  , isFinal
  , isStart
  , automatonToDot
  , automatonToDotClustered
  , accept
  ) where

import           Data.Graph.Inductive
import           Data.GraphViz
import           Data.GraphViz.Attributes.Complete
import qualified Data.Set                          as Set
import qualified Data.Text                         as T

import           Data.Graph.Inductive.Query.DFS    (scc)
import qualified Data.Text.Lazy                    as TL

import           Data.List                         (findIndex, foldl')
import           Data.Maybe                        (fromJust, fromMaybe)

data NFA state transition = NFA
  { sigma   :: Set.Set transition
  , etats   :: Set.Set state
  , premier :: Set.Set state
  , final   :: Set.Set state
  , delta   :: state -> transition -> Set.Set state
  }

isFinal :: Ord state => NFA state transition -> state -> Bool
isFinal = flip Set.member . final

isStart :: Ord state => NFA state transition -> state -> Bool
isStart = flip Set.member . premier

accept ::
     forall state transition. Ord state
  => NFA state transition
  -> [transition]
  -> Bool
accept (NFA _ _ prem fin delt) l =
  not $ Set.null $ Set.intersection fin $ foldl' accept' prem l
  where
    accept' :: Set.Set state -> transition -> Set.Set state
    accept' s t = foldr (\s' acc -> Set.union acc $ delt s' t) Set.empty s

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
    stateIndex state =
      Data.Maybe.fromMaybe (-1) (lookup state $ zip states indices)
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
              , Color [toWColor Red]
              , Style [SItem Filled []]
              ]
        , fmtEdge = \(_, _, l) -> [Label $ StrLabel $ TL.fromStrict l]
        }
    shapeOf (val, _)
      | isFinal a val = DoubleCircle
      | otherwise = Circle
    colorOf (val, _)
      | isStart a val = Green
      | otherwise = White

automatonToDotClustered ::
     Show transition => NFA Node transition -> DotGraph Node
automatonToDotClustered a = graphToDot params graph
  where
    graph = automataToGraph a
    sccs = Data.Graph.Inductive.Query.DFS.scc graph
    params =
      defaultParams
        { globalAttributes = [GraphAttrs [RankDir FromLeft]]
        , fmtNode =
            \node ->
              [ Shape $ shapeOf node
              , FillColor [toWColor $ colorOf node]
              -- , Color [toWColor Red]
              , Style [SItem Filled []]
              ]
        , fmtEdge = \(_, _, l) -> [Label $ StrLabel $ TL.fromStrict l]
        , isDotCluster = const True
        , clusterBy = clusterLogic
        , clusterID = Num . Int
        }
    clusterLogic (n, l) = C (nodeClusterId n) $ N (n, l)
    nodeClusterId node = fromJust $ findIndex (elem node) sccs
    shapeOf (val, _)
      | isFinal a val = DoubleCircle
      | otherwise = Circle
    colorOf (val, _)
      | isStart a val = Green
      | otherwise = White