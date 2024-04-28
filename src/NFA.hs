module NFA
  ( NFA(..)
  , automataToGraph
  , isFinal
  , isStart
  , automatonToDot
  ) where

import           Data.Graph.Inductive
import           Data.Maybe                        (fromMaybe)
import qualified Data.Set                          as Set
import qualified Data.Text                         as T
import qualified Data.Text.Lazy                    as LT

import           Data.GraphViz
import           Data.GraphViz.Attributes.Complete

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
              ]
        , fmtEdge = \(_, _, l) -> [Label $ StrLabel $ LT.fromStrict l]
        }
    shapeOf (val, _)
      | isFinal a val = DoubleCircle
      | otherwise = Circle
    colorOf (val, _)
      | isStart a val = Green
      | otherwise = White
