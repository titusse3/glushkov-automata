module NFAF
  ( NFAF(..)
  ) where

import qualified Data.Graph.Inductive              as Gr
import           Data.Graph.Inductive.Query.DFS    (scc)
import           Data.GraphViz
import           Data.GraphViz.Attributes.Complete
import           Data.List                         (findIndex, intercalate)
import qualified Data.Map                          as Map
import           Data.Maybe                        (fromJust, fromMaybe, isJust)
import qualified Data.Set                          as Set
import qualified Data.Text                         as T
import qualified Data.Text.Lazy                    as TL
import           NFA

data NFAF state transition = NFAF
  { sigma   :: Set.Set transition
  , etats   :: Set.Set state
  , premier :: Set.Set state
  , final   :: Set.Set state
  , delta   :: state -> transition -> Set.Set state
  }

instance (Ord state, Show state, Ord transition, Show transition) =>
         Show (NFAF state transition) where
  show (NFAF s q p f d) =
    "NFAF {\n"
      ++ "  sigma = "
      ++ show s
      ++ ",\n"
      ++ "  etats = "
      ++ show q
      ++ ",\n"
      ++ "  premier = "
      ++ show p
      ++ ",\n"
      ++ "  final = "
      ++ show f
      ++ ",\n"
      ++ "  delta = "
      ++ showDelta d q s
      ++ "\n"
      ++ "}"

showDelta ::
     (Show transition, Show state)
  => (state -> transition -> Set.Set state)
  -> Set.Set state
  -> Set.Set transition
  -> String
showDelta d states transitions =
  let transitionsList =
        [(q, t, d q t) | q <- Set.toList states, t <- Set.toList transitions]
      showTransition (q, t, qs) =
        show q ++ " -- " ++ show t ++ " -> " ++ show (Set.toList qs)
   in "{\n" ++ intercalate ",\n" (map showTransition transitionsList) ++ "\n}"

automataToGraph ::
     (Ord state, Show state, Show transition)
  => NFAF state transition
  -> Gr.Gr T.Text T.Text
automataToGraph a = Gr.mkGraph nodesList edgesList
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

transExist :: Ord state => NFAF state transition -> state -> state -> Bool
transExist (NFAF sig _ _ _ delt) s s' =
  foldl (\b a -> b || Set.member s' (delt s a)) False sig

instance (Ord state, Ord transition, Show state, Show transition) =>
         NFA (NFAF state transition) where
  type StateType (NFAF state transition) = state
  type TransitionType (NFAF state transition) = transition
  emptyNFA :: NFAF state transition
  emptyNFA = NFAF Set.empty Set.empty Set.empty Set.empty (\_ _ -> Set.empty)
  addState ::
       StateType (NFAF state transition)
    -> NFAF state transition
    -> Maybe (NFAF state transition)
  addState s (NFAF sig etat prem fin delt) =
    if Set.member s etat
      then Nothing
      else Just $ NFAF sig (Set.insert s etat) prem fin delt
  addTransition ::
       ( StateType (NFAF state transition)
       , StateType (NFAF state transition)
       , TransitionType (NFAF state transition))
    -> NFAF state transition
    -> Maybe (NFAF state transition)
  addTransition (s1, s2, t) (NFAF sig etat prem fin delt) =
    if Set.member s1 etat && Set.member s2 etat
      then Just
             $ NFAF
                 (Set.insert t sig)
                 etat
                 prem
                 fin
                 (\s t' ->
                    if s == s1 && t == t'
                      then Set.insert s2 (delt s t')
                      else delt s t')
      else Nothing
  stateExist ::
       StateType (NFAF state transition) -> NFAF state transition -> Bool
  stateExist s (NFAF _ etat _ _ _) = Set.member s etat
  isFinal :: StateType (NFAF state transition) -> NFAF state transition -> Bool
  isFinal s (NFAF _ _ _ fin _) = Set.member s fin
  isStart :: StateType (NFAF state transition) -> NFAF state transition -> Bool
  isStart s (NFAF _ _ prem _ _) = Set.member s prem
  initialStates ::
       NFAF state transition -> Set.Set (StateType (NFAF state transition))
  initialStates = premier
  finalStates ::
       NFAF state transition -> Set.Set (StateType (NFAF state transition))
  finalStates = final
  getStates :: NFAF state transition -> [StateType (NFAF state transition)]
  getStates (NFAF _ q _ _ _) = Set.toList q
  transitionExist ::
       ( StateType (NFAF state transition)
       , StateType (NFAF state transition)
       , TransitionType (NFAF state transition))
    -> NFAF state transition
    -> Bool
  transitionExist (s1, s2, t) (NFAF _ _ _ _ delt) = Set.member s2 (delt s1 t)
  hasEdge ::
       (StateType (NFAF state transition), StateType (NFAF state transition))
    -> NFAF state transition
    -> Bool
  hasEdge (s1, s2) (NFAF sig _ _ _ delt) =
    any (Set.member s2 . delt s1) (Set.toList sig)
  removeState ::
       StateType (NFAF state transition)
    -> NFAF state transition
    -> Maybe (NFAF state transition)
  removeState s (NFAF sig etat prem fin delt) =
    if Set.member s etat
      then let etats' = Set.delete s etat
               prem' = Set.delete s prem
               fin' = Set.delete s fin
               delt' s' t' =
                 if s' == s
                   then Set.empty
                   else Set.delete s (delt s' t')
            in Just $ NFAF sig etats' prem' fin' delt'
      else Nothing
  removeTransition ::
       ( StateType (NFAF state transition)
       , StateType (NFAF state transition)
       , TransitionType (NFAF state transition))
    -> NFAF state transition
    -> Maybe (NFAF state transition)
  removeTransition (s1, s2, t) (NFAF sig etat prem fin delt) =
    if Set.member s1 etat && Set.member s2 etat
      then let delt' s' t' =
                 if s' == s1 && t' == t
                   then Set.delete s2 (delt s' t')
                   else delt s' t'
            in Just $ NFAF sig etat prem fin delt'
      else Nothing
  removeTransitions ::
       (StateType (NFAF state transition), StateType (NFAF state transition))
    -> NFAF state transition
    -> Maybe (NFAF state transition)
  removeTransitions (s1, s2) (NFAF sig etat prem fin delt) =
    if Set.member s1 etat && Set.member s2 etat
      then let delt' s' t' =
                 if s' == s1
                   then Set.delete s2 (delt s' t')
                   else delt s' t'
            in Just $ NFAF sig etat prem fin delt'
      else Nothing
  makeInit ::
       StateType (NFAF state transition)
    -> NFAF state transition
    -> Maybe (NFAF state transition)
  makeInit s (NFAF sig etat prem fin delt) =
    if Set.member s etat
      then Just $ NFAF sig etat (Set.insert s prem) fin delt
      else Nothing
  makeFinal ::
       StateType (NFAF state transition)
    -> NFAF state transition
    -> Maybe (NFAF state transition)
  makeFinal s (NFAF sig etat prem fin delt) =
    if Set.member s etat
      then Just $ NFAF sig etat prem (Set.insert s fin) delt
      else Nothing
  isHomogeneous :: NFAF state transition -> Bool
  isHomogeneous (NFAF sig etat _ _ delt) =
    isJust
      $ foldl
          (\s lettre -> do
             let inner =
                   foldl (\s' t -> Set.union (delt t lettre) s') Set.empty etat
             if isJust s && Set.disjoint inner (fromJust s)
               then pure $ Set.union inner $ fromJust s
               else Nothing)
          (Just Set.empty)
          sig
  isStandard :: NFAF state transition -> Bool
  isStandard a =
    hasOneElem
      && foldl (\acc q' -> acc || not (transExist a q' (head i))) False q
    where
      q = etats a
      i = Set.toList $ premier a
      hasOneElem =
        case i of
          []  -> False
          [_] -> True
          _   -> False
  makeStandard ::
       Enum (StateType (NFAF state transition))
    => NFAF state transition
    -> NFAF state transition
  makeStandard (NFAF sig etat prem fin delt) =
    let i' = toEnum (Set.size etat)
        etats' = Set.insert i' etat
        prem' = Set.singleton i'
        fin' =
          if null $ Set.intersection prem fin
            then fin
            else Set.insert i' fin
        delt' s t
          | s == i' = Set.unions [delt p t | p <- Set.toList prem]
          | otherwise = delt s t
     in NFAF sig etats' prem' fin' delt'
  extractListStateAutomata ::
       Set.Set (StateType (NFAF state transition))
    -> NFAF state transition
    -> Maybe (NFAF state transition)
  extractListStateAutomata subset (NFAF sig etat prem fin delt) =
    if Set.isSubsetOf subset etat
      then let etats' = subset
               prem' = Set.intersection prem subset
               fin' = Set.intersection fin subset
               delt' s t =
                 if Set.member s subset
                   then Set.intersection subset (delt s t)
                   else Set.empty
            in Just $ NFAF sig etats' prem' fin' delt'
      else Nothing
  maximalOrbits ::
       NFAF state transition -> [Set.Set (StateType (NFAF state transition))]
  maximalOrbits a =
    map (Set.fromList . map (fromJust . flip Map.lookup mapNode)) orbitM
    where
      graph = automataToGraph a
      sccs = scc graph
      filterFun []  = False
      filterFun [x] = Gr.hasEdge graph (x, x)
      filterFun _   = True
      orbitM = filter filterFun sccs
      states = Set.toList $ etats a
      mapNode :: Map.Map Gr.Node state
      mapNode = Map.fromList [(stateIndex s, s) | s <- states]
      stateIndex state = fromMaybe (-1) (lookup state $ zip states indices)
      indices = [0 ..]
  isOrbit ::
       Set.Set (StateType (NFAF state transition))
    -> NFAF state transition
    -> Bool
  isOrbit o a = firstTest l && isJust g && hasOneElem fc
    where
      l = Set.toList o
      firstTest []  = False
      firstTest [x] = transExist a x x
      firstTest _   = True
      hasOneElem [_] = True
      hasOneElem _   = False
      a' = extractListStateAutomata o a
      g = automataToGraph <$> a'
      fc = scc $ fromJust g
  directSucc ::
       StateType (NFAF state transition)
    -> NFAF state transition
    -> Set.Set (StateType (NFAF state transition))
  directSucc s a =
    foldl (\acc t -> Set.union acc $ delta a s t) Set.empty $ sigma a
  directPred ::
       StateType (NFAF state transition)
    -> NFAF state transition
    -> Set.Set (StateType (NFAF state transition))
  directPred s (NFAF sig etat _ _ delt) =
    foldl
      (\acc t ->
         Set.union acc (Set.filter (\s' -> Set.member s (delt s' t)) etat))
      Set.empty
      sig
  orbitIn ::
       Set.Set (StateType (NFAF state transition))
    -> NFAF state transition
    -> Set.Set (StateType (NFAF state transition))
  orbitIn o a =
    if isOrbit o a
      then foldl f Set.empty o
      else Set.empty
    where
      f s x =
        if Set.member x (premier a)
             || Set.difference (directPred x a) o /= Set.empty
          then Set.insert x s
          else s
  orbitOut ::
       Set.Set (StateType (NFAF state transition))
    -> NFAF state transition
    -> Set.Set (StateType (NFAF state transition))
  orbitOut o a =
    if isOrbit o a
      then foldl f Set.empty o
      else Set.empty
    where
      f s x =
        if Set.member x (final a)
             || Set.difference (directSucc x a) o /= Set.empty
          then Set.insert x s
          else s
  isStableOrbit ::
       Set.Set (StateType (NFAF state transition))
    -> NFAF state transition
    -> Bool
  isStableOrbit o a =
    isOrbit o a && inOut == filter (uncurry (transExist a)) inOut
    where
      inO = orbitIn o a
      outO = orbitOut o a
      inOut = do
        x <- Set.toList outO
        y <- Set.toList inO
        return (x, y)
  isStronglyStableOrbit ::
       Set.Set (StateType (NFAF state transition))
    -> NFAF state transition
    -> Bool
  isStronglyStableOrbit o a =
    isOrbit o a
      && (isStableOrbit o a
            && foldl
                 (\acc o' -> acc && isStronglyStableOrbit o' a')
                 True
                 (maximalOrbits a'))
    where
      autoOrbit = fromJust $ extractListStateAutomata o a
      inO = orbitIn o a
      outO = orbitOut o a
      outIn = do
        x <- Set.toList outO
        y <- Set.toList inO
        return (x, y)
      a' =
        foldl
          (\n (x, x') ->
             case removeTransitions (x, x') n of
               Just n' -> n'
               _       -> n)
          autoOrbit
          outIn
  isTransversOrbit ::
       Set.Set (StateType (NFAF state transition))
    -> NFAF state transition
    -> Bool
  isTransversOrbit o a = isOrbit o a && Set.size sIn <= 1 && Set.size sOut <= 1
    where
      oIn = orbitIn o a
      oOut = orbitOut o a
      sIn = Set.map (\x -> Set.difference (directPred x a) o) oIn
      sOut = Set.map (\x -> Set.difference (directSucc x a) o) oOut
  isStronglyTransversOrbit ::
       Set.Set (StateType (NFAF state transition))
    -> NFAF state transition
    -> Bool
  isStronglyTransversOrbit o a =
    isOrbit o a
      && (isTransversOrbit o a
            && foldl
                 (\acc o' -> acc && isStronglyStableOrbit o' a')
                 True
                 (maximalOrbits a'))
    where
      autoOrbit = fromJust $ extractListStateAutomata o a
      inO = orbitIn o a
      outO = orbitOut o a
      outIn = do
        x <- Set.toList outO
        y <- Set.toList inO
        return (x, y)
      a' =
        foldl
          (\n (x, x') ->
             case removeTransitions (x, x') n of
               Just n' -> n'
               _       -> n)
          autoOrbit
          outIn
  accept ::
       [TransitionType (NFAF state transition)] -> NFAF state transition -> Bool
  accept l (NFAF _ _ prem fin delt) =
    not $ Set.null $ Set.intersection fin $ foldl accept' prem l
    where
      accept' :: Set.Set state -> transition -> Set.Set state
      accept' s t = foldr (\s' acc -> Set.union acc $ delt s' t) Set.empty s
  automatonToDot :: NFAF state transition -> DotGraph Gr.Node
  automatonToDot a = graphToDot params graph
    where
      graph = automataToGraph a
      params =
        nonClusteredParams
          { globalAttributes =
              [GraphAttrs [RankDir FromLeft, BgColor [toWC $ RGBA 0 0 0 0]]]
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
      mapNode :: Map.Map Gr.Node state
      mapNode = Map.fromList [(stateIndex s, s) | s <- states]
      stateIndex state = fromMaybe (-1) (lookup state $ zip states indices)
      indices = [0 ..]
      shapeOf (val, _) =
        if isFinal n a
          then DoubleCircle
          else Circle
        where
          n = fromJust $ Map.lookup val mapNode
      colorOf (val, _) =
        if isStart n a
          then Green
          else White
        where
          n = fromJust $ Map.lookup val mapNode
  automatonToDotClustered ::
       [Set.Set (StateType (NFAF state transition))]
    -> NFAF state transition
    -> DotGraph Gr.Node
  automatonToDotClustered ls a = graphToDot params graph
    where
      graph = automataToGraph a
      params =
        defaultParams
          { globalAttributes =
              [GraphAttrs [RankDir FromLeft, BgColor [toWC $ RGBA 0 0 0 0]]]
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
      mapNode :: Map.Map Gr.Node state
      mapNode = Map.fromList [(stateIndex s, s) | s <- states]
      stateIndex state = fromMaybe (-1) (lookup state $ zip states indices)
      indices = [0 ..]
      shapeOf (val, _) =
        if isFinal n a
          then DoubleCircle
          else Circle
        where
          n = fromJust $ Map.lookup val mapNode
      colorOf (val, _) =
        if isStart n a
          then Green
          else White
        where
          n = fromJust $ Map.lookup val mapNode
