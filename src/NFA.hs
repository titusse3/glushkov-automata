module NFA
  ( NFA(..)
  , automataToGraph
  , isFinal
  , isStart
  , automatonToDot
  , automatonToDotClustered
  , accept
  , addTransition
  , removeTransition
  , isHomogeneous
  , addState
  , removeState
  ) where

import           Data.Graph.Inductive
import           Data.GraphViz
import           Data.GraphViz.Attributes.Complete
import qualified Data.Set                          as Set
import qualified Data.Text                         as T

import           Data.Graph.Inductive.Query.DFS    (scc)
import qualified Data.Text.Lazy                    as TL

import           Data.List                         (findIndex, foldl')
import           Data.Maybe                        (fromJust, fromMaybe,
                                                    isNothing)

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

isHomogeneous :: Ord a => NFA a transition -> Bool
isHomogeneous (NFA sig etat _ _ delt) =
  not . isNothing
    $ foldl
        (\s lettre -> do
           let inner =
                 foldl (\s' t -> Set.union (delt t lettre) s') Set.empty etat
           if not (isNothing s) && (Set.disjoint inner $ fromJust s)
             then pure $ Set.union inner $ fromJust s
             else Nothing)
        (Just Set.empty)
        sig

addState :: Ord state => NFA state transition -> state -> NFA state transition
addState (NFA sig e prem fin delt) s = NFA sig (Set.insert s e) prem fin delt

removeState ::
     Ord state => NFA state transition -> state -> NFA state transition
removeState (NFA sig e prem fin delt) s =
  if Set.member s e
    then let e' = Set.delete s e
             p = Set.delete s prem
             f = Set.delete s fin
             update n a =
               if n == s
                 then Set.empty
                 else Set.delete s $ delt n a
          in NFA sig e' p f update
    else NFA sig e prem fin delt

addTransition ::
     (Ord transition, Ord state)
  => NFA state transition
  -> (state, state, transition)
  -> NFA state transition
addTransition (NFA sig e prem fin delt) (i, o, l) =
  if Set.member i e && Set.member o e
    then NFA sig' e prem fin delt'
    else NFA sig e prem fin delt
  where
    sig' = Set.insert l sig
    delt' =
      (\s t ->
         if s == i && l == t
           then Set.insert o $ delt s t
           else delt s t)

removeTransition ::
     (Ord transition, Ord state)
  => NFA state transition
  -> (state, state, transition)
  -> NFA state transition
removeTransition (NFA sig e prem fin delt) (i, o, l) =
  if Set.member i e && Set.member o e
    then NFA sig e prem fin delt'
    else NFA sig e prem fin delt
  where
    delt' =
      (\s t ->
         if s == i && l == t
           then Set.delete o $ delt s t
           else delt s t)

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
    nodesList = [(stateIndex s, T.pack $ show s) | s <- states]
    edgesList =
      [ (stateIndex s, stateIndex t, formatText $ T.pack $ show tr)
      | (s, trs) <- stateTrans
      , tr <- Set.toList trs
      , t <- Set.toList (deltaFun s tr)
      ]
    states = Set.toList $ etats a
    stateIndex state =
      Data.Maybe.fromMaybe (-1) (lookup state $ zip states indices)
    indices = [0 ..]
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
              , Label $ StrLabel $ TL.fromStrict $ snd node
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
              , Style [SItem Filled []]
              , Label $ StrLabel $ TL.fromStrict $ snd node
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
