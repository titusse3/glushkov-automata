module NFA
  ( NFA(..)
  , Orbit
  , orbitToText
  , automataToGraph
  , isFinal
  , isStart
  , automatonToDot
  , automatonToDotClustered
  , accept
  , addTransition
  , removeTransition
  , isHomogeneous
  , isStandard
  , addState
  , removeState
  , makeFinal
  , makeInit
  , directSucc
  , directPred
  , maximalOrbits
  , orbitIn
  , orbitOut
  , isStableOrbit
  , extractListStateAutomata
  , isStronglyStableOrbit
  , isTransversOrbit
  , isStronglyTransversOrbit
  , isOrbit
  ) where

import           Data.Graph.Inductive
import           Data.Graph.Inductive.Query.DFS    (scc)
import           Data.GraphViz
import           Data.GraphViz.Attributes.Complete
import           Data.List                         (findIndex, intercalate)
import qualified Data.Map                          as Map
import           Data.Maybe                        (fromJust, fromMaybe, isJust,
                                                    isNothing)
import qualified Data.Set                          as Set
import qualified Data.Text                         as T
import qualified Data.Text.Lazy                    as TL

import Debug.Trace

type Orbit state = Set.Set state

data NFA state transition = NFA
  { sigma   :: Set.Set transition
  , etats   :: Set.Set state
  , premier :: Set.Set state
  , final   :: Set.Set state
  , delta   :: state -> transition -> Set.Set state
  }

-- passe au garage de l'automateMap.Map
-- se reisenger sur la memoization
-- f :: NFA state transition -> Map.Map (lettre, etat) (Map.Map etat) -> f'
instance (Show state, Show transition, Ord state, Ord transition) =>
         Show (NFA state transition) where
  show nfa =
    "Sigma: "
      ++ showSet (sigma nfa)
      ++ "\n"
      ++ "Etats: "
      ++ showSet (etats nfa)
      ++ "\n"
      ++ "Premier: "
      ++ showSet (premier nfa)
      ++ "\n"
      ++ "Final: "
      ++ showSet (final nfa)
      ++ "\n"
      ++ "Delta:\n"
      ++ showDelta (etats nfa) (sigma nfa) (delta nfa)
    where
      showSet :: (Show a) => Set.Set a -> String
      showSet = show . Set.toList
      showDelta ::
           Set.Set state
        -> Set.Set transition
        -> (state -> transition -> Set.Set state)
        -> String
      showDelta states transitions deltaFun =
        intercalate
          "\n"
          [ "  δ("
            ++ show s
            ++ ", "
            ++ show t
            ++ ") = "
            ++ showSet (deltaFun s t)
          | s <- Set.toList states
          , t <- Set.toList transitions
          ]

orbitToText :: (Show state) => (Orbit state) -> T.Text
orbitToText o =
  mconcat ["{", (T.intercalate "," $ map (T.pack . show) $ Set.toList o), "}"]

isFinal :: Ord state => NFA state transition -> state -> Bool
isFinal = flip Set.member . final

isStart :: Ord state => NFA state transition -> state -> Bool
isStart = flip Set.member . premier

-- faire doc
isHomogeneous :: Ord state => NFA state transition -> Bool
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

isStandard :: Ord state => NFA state transition -> Bool
isStandard a =
  hasOneElem && foldl (\acc q' -> acc || (transExist a q' $ head i)) False q
  where
    q = etats a
    i = Set.toList $ premier a
    hasOneElem =
      case i of
        []  -> False
        [_] -> True
        _   -> False

transExist :: Ord state => NFA state transition -> state -> state -> Bool
transExist (NFA sig _ _ _ delt) s s' =
  foldl (\b a -> b || (Set.member s' $ delt s a)) False sig

makeFinal ::
     Ord state => NFA state transition -> state -> Maybe (NFA state transition)
makeFinal (NFA sig e prem fin delt) s =
  if Set.member s e
    then Just a
    else Nothing
  where
    a = (NFA sig e prem (Set.insert s fin) delt)

makeInit ::
     Ord state => NFA state transition -> state -> Maybe (NFA state transition)
makeInit (NFA sig e prem fin delt) s =
  if Set.member s e
    then Just a
    else Nothing
  where
    a = (NFA sig e (Set.insert s prem) fin delt)

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

-- mémoriwation des fonctions
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

-- pas perfomant, en parler dans le rapport
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

removeTransitions ::
     (Ord transition, Ord state)
  => NFA state transition
  -> (state, state)
  -> NFA state transition
removeTransitions a (x, x') =
  foldl (\n l -> removeTransition n (x, x', l)) a $ sigma a

directSucc :: Ord state => NFA state transition -> state -> Set.Set state
directSucc (NFA sig _ _ _ delt) e =
  foldl (\s a -> Set.union s (delt e a)) Set.empty sig

directPred :: Ord state => NFA state transition -> state -> Set.Set state
directPred (NFA sig etat _ _ delt) e =
  foldl
    (\s a -> Set.union s $ Set.filter (\q -> Set.member e $ delt q a) etat)
    Set.empty
    sig

extractListStateAutomata ::
     Ord state
  => NFA state transition
  -> Orbit state
  -> Maybe (NFA state transition)
extractListStateAutomata (NFA sig q prem fin delt) o =
  if Set.isSubsetOf o q
    then let e' = o
             prem' = Set.intersection prem o
             fin' = Set.intersection fin o
             delt' s t =
               if Set.member s o
                 then Set.intersection o $ delt s t
                 else Set.empty
          in pure (NFA sig e' prem' fin' delt')
    else Nothing

maximalOrbits ::
     forall state transition. (Ord state, Show state, Show transition)
  => NFA state transition
  -> [Orbit state]
maximalOrbits a =
  map (\l -> Set.fromList $ map (fromJust . flip Map.lookup mapNode) l) orbitM
  where
    graph = automataToGraph a
    sccs = Data.Graph.Inductive.Query.DFS.scc graph
    filterFun []     = False
    filterFun (x:[]) = hasEdge graph (x, x)
    filterFun _      = True
    orbitM = filter filterFun sccs
    states = Set.toList $ etats a
    mapNode :: Map.Map Node state
    mapNode = Map.fromList [(stateIndex s, s) | s <- states]
    stateIndex state =
      Data.Maybe.fromMaybe (-1) (lookup state $ zip states indices)
    indices = [0 ..]

isOrbit ::
     (Ord state, Show state, Show transition)
  => NFA state transition
  -> Orbit state
  -> Bool
isOrbit a o = firstTest l && isJust g && hasOneElem fc
  where
    l = Set.toList o
    firstTest []  = False
    firstTest [x] = transExist a x x
    firstTest _   = True
    hasOneElem [_] = True
    hasOneElem _   = False
    a' = extractListStateAutomata a o
    g = automataToGraph <$> a'
    fc = fromJust $ Data.Graph.Inductive.Query.DFS.scc <$> g

orbitIn ::
     forall state transition. (Ord state, Show state, Show transition)
  => NFA state transition
  -> Orbit state
  -> Set.Set state
orbitIn a o =
  if isOrbit a o
    then foldl f Set.empty o
    else Set.empty
  where
    f s x =
      if Set.member x (premier a)
           || Set.difference (directPred a x) o /= Set.empty
        then Set.insert x s
        else s

orbitOut ::
     forall state transition. (Ord state, Show state, Show transition)
  => NFA state transition
  -> Orbit state
  -> Set.Set state
orbitOut a o =
  if isOrbit a o
    then foldl f Set.empty o
    else Set.empty
  where
    f s x =
      if Set.member x (final a)
           || Set.difference (directSucc a x) o /= Set.empty
        then Set.insert x s
        else s

isStableOrbit ::
     (Ord state, Show state, Show transition)
  => NFA state transition
  -> Orbit state
  -> Bool
isStableOrbit a o =
  isOrbit a o && inOut == filter (\(x, x') -> transExist a x x') inOut
  where
    inO = trace (show $ orbitIn a o) $ orbitIn a o
    outO = trace (show $ orbitOut a o) $ orbitOut a o
    inOut = do
      x <- Set.toList outO
      y <- Set.toList inO
      return (x, y)

isStronglyStableOrbit ::
     (Ord state, Ord transition, Show state, Show transition)
  => NFA state transition
  -> Orbit state
  -> Bool
isStronglyStableOrbit a o =
  isOrbit a o
    && if not $ isStableOrbit a o
         then False
         else foldl (\acc o' -> acc && isStronglyStableOrbit a' o') True
                $ maximalOrbits autoOrbit
  where
    autoOrbit = fromJust $ extractListStateAutomata a o
    inO = orbitIn a o
    outO = orbitOut a o
    outIn = do
      x <- Set.toList outO
      y <- Set.toList inO
      return (x, y)
    a' =
      foldl
        (\n (x, x') -> removeTransitions n (x, x'))
        (fromJust $ extractListStateAutomata a o)
        outIn

isTransversOrbit ::
     (Ord state, Show state, Show transition)
  => NFA state transition
  -> Orbit state
  -> Bool
isTransversOrbit a o =
  isOrbit a o
    && all (not . null) [lOut, lIn]
    && all (== head lOut) (tail lOut)
    && all (== head lIn) (tail lIn)
  where
    lOut = map (directSucc a) $ Set.toList $ orbitOut a o
    lIn = map (directPred a) $ Set.toList $ orbitIn a o

isStronglyTransversOrbit ::
     (Ord state, Ord transition, Show state, Show transition)
  => NFA state transition
  -> Orbit state
  -> Bool
isStronglyTransversOrbit a o =
  isOrbit a o
    && if not $ isTransversOrbit a o
         then False
         else foldl (\acc o' -> acc && isStronglyStableOrbit a' o') True
                $ maximalOrbits autoOrbit
  where
    autoOrbit = fromJust $ extractListStateAutomata a o
    inO = orbitIn a o
    outO = orbitOut a o
    outIn = do
      x <- Set.toList outO
      y <- Set.toList inO
      return (x, y)
    a' =
      foldl
        (\n (x, x') -> removeTransitions n (x, x'))
        (fromJust $ extractListStateAutomata a o)
        outIn

accept ::
     forall state transition. Ord state
  => NFA state transition
  -> [transition]
  -> Bool
accept (NFA _ _ prem fin delt) l =
  not $ Set.null $ Set.intersection fin $ foldl accept' prem l
  where
    accept' :: Set.Set state -> transition -> Set.Set state
    accept' s t = foldr (\s' acc -> Set.union acc $ delt s' t) Set.empty s

-- new type, dérivation pour enlever les "", redéfinir le show
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
    stateIndex state = fromJust $ lookup state $ zip states indices
    indices = [0 ..]
    stateTrans = [(s, sigma a) | s <- states]
    deltaFun = delta a
    formatText = T.filter (/= '\'')

automatonToDot ::
     forall state transition. (Show transition, Show state, Ord state)
  => NFA state transition
  -> DotGraph Node
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
    states = Set.toList $ etats a
    mapNode :: Map.Map Node state
    mapNode = Map.fromList [(stateIndex s, s) | s <- states]
    stateIndex state =
      Data.Maybe.fromMaybe (-1) (lookup state $ zip states indices)
    indices = [0 ..]
    shapeOf (val, _) =
      if isFinal a n
        then DoubleCircle
        else Circle
      where
        n = (fromJust $ Map.lookup val mapNode)
    colorOf (val, _) =
      if isStart a n
        then Green
        else White
      where
        n = (fromJust $ Map.lookup val mapNode)

automatonToDotClustered ::
     forall state transition. (Show transition, Show state, Ord state)
  => NFA state transition
  -> [Set.Set state]
  -> DotGraph Node
automatonToDotClustered a ls = graphToDot params graph
  where
    graph = automataToGraph a
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
    clusterLogic (n, l) =
      case nodeClusterId n of
        Just cid -> C cid $ N (n, l)
        Nothing  -> N (n, l)
    nodeClusterId node = findIndex (Set.member n') ls
      where
        n' = fromJust $ Map.lookup node mapNode
    states = Set.toList $ etats a
    mapNode :: Map.Map Node state
    mapNode = Map.fromList [(stateIndex s, s) | s <- states]
    stateIndex state =
      Data.Maybe.fromMaybe (-1) (lookup state $ zip states indices)
    indices = [0 ..]
    shapeOf (val, _) =
      if isFinal a n
        then DoubleCircle
        else Circle
      where
        n = (fromJust $ Map.lookup val mapNode)
    colorOf (val, _) =
      if isStart a n
        then Green
        else White
      where
        n = (fromJust $ Map.lookup val mapNode)
