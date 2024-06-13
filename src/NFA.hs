module NFA
  ( NFA(..)
  , orbitToText
  , emptyNFA
  , addState
  , makeInit
  , makeFinal
  , addTransition
  , transitionExist
  , hasEdge
  , removeState
  , removeTransition
  , removeTransitions
  , isStart
  , isFinal
  , isStandard
  , isHomogeneous
  , makeStandard
  , directSucc
  , directPred
  , extractListStateAutomata
  , maximalOrbits
  , isOrbit
  , orbitIn
  , orbitOut
  , isStableOrbit
  , isStronglyStableOrbit
  , isTransversOrbit
  , isStronglyTransversOrbit
  , accept
  , automatonToDot
  , automatonToDotClustered
  ) where

import qualified Data.Graph.Inductive              as Gr
import           Data.Graph.Inductive.Query.DFS
import           Data.GraphViz
import           Data.GraphViz.Attributes.Complete
import qualified Data.List                         as L
import qualified Data.Map                          as Map
import           Data.Maybe
import qualified Data.Set                          as Set
import qualified Data.Text                         as T
import qualified Data.Text.Lazy                    as TL

import           Debug.Trace

data NFA state transition = NFA
  { sigma   :: Set.Set transition
  , etats   :: Map.Map state Int
  , premier :: Set.Set Int
  , final   :: Set.Set Int
  , graph   :: Gr.Gr state transition
  , lastN   :: Int
  }

instance (Show state, Show transition) => Show (NFA state transition) where
  show (NFA sigma etats premier final graph lastN) =
    "NFA {\n"
      ++ "  Sigma: "
      ++ showSet sigma
      ++ "\n"
      ++ "  Etats: "
      ++ showMap etats
      ++ "\n"
      ++ "  Premier: "
      ++ showSet premier
      ++ "\n"
      ++ "  Final: "
      ++ showSet final
      ++ "\n"
      ++ "  Graph: "
      ++ showGraph graph
      ++ "\n"
      ++ "  LastN: "
      ++ show lastN
      ++ "\n"
      ++ "}"
    where
      showSet s = "{" ++ L.intercalate ", " (map show $ Set.toList s) ++ "}"
      showMap m =
        "{"
          ++ L.intercalate
               ", "
               [show k ++ ": " ++ show v | (k, v) <- Map.toList m]
          ++ "}"
      showGraph g =
        "{" ++ L.intercalate ", " (map showEdge $ Gr.labEdges g) ++ "}"
      showEdge (s, t, l) =
        "(" ++ show s ++ " -" ++ show l ++ "-> " ++ show t ++ ")"

orbitToText :: (Show state) => (Set.Set state) -> T.Text
orbitToText o =
  mconcat ["{", (T.intercalate "," $ map (T.pack . show) $ Set.toList o), "}"]

emptyNFA :: NFA state transition
emptyNFA = NFA Set.empty Map.empty Set.empty Set.empty Gr.empty 0

addState ::
     Ord state => state -> NFA state transition -> Maybe (NFA state transition)
addState st (NFA s q i f g n) =
  if stateExist st (NFA s q i f g n)
    then Nothing
    else let q' = Map.insert st (n + 1) q
             g' = Gr.insNode (n + 1, st) g
          in pure $ NFA s q' i f g' (n + 1)

addTransition ::
     (Ord state, Ord transition)
  => (state, state, transition)
  -> NFA state transition
  -> Maybe (NFA state transition)
addTransition (q0, q1, t) (NFA s q i f g n) =
  case transformTuple (Map.lookup q0 q, Map.lookup q1 q, t) of
    Just (n0, n1, trans) ->
      let g' = Gr.insEdge (n0, n1, trans) g
          s' = Set.insert trans s
       in pure $ NFA s' q i f g' n
    Nothing -> Nothing

stateExist :: Ord state => state -> NFA state transition -> Bool
stateExist st = Map.member st . etats

isFinal :: Ord state => state -> NFA state transition -> Bool
isFinal lbl nfa =
  case Map.lookup lbl (etats nfa) of
    Just index -> Set.member index (final nfa)
    Nothing    -> False

isStart :: Ord state => state -> NFA state transition -> Bool
isStart lbl nfa =
  case Map.lookup lbl (etats nfa) of
    Just index -> Set.member index (premier nfa)
    Nothing    -> False

transformTuple :: (Maybe Int, Maybe Int, a) -> Maybe (Int, Int, a)
transformTuple (m1, m2, a) = do
  v1 <- m1
  v2 <- m2
  return (v1, v2, a)

transitionExist ::
     (Ord state, Eq transition)
  => (state, state, transition)
  -> NFA state transition
  -> Bool
transitionExist (q0, q1, t) (NFA _ q _ _ g _) =
  let n0 = Map.lookup q0 q
      n1 = Map.lookup q1 q
      s = Gr.hasLEdge g <$> transformTuple (n0, n1, t)
   in isJust s && fromJust s

hasEdge :: Ord state => (state, state) -> NFA state transition -> Bool
hasEdge (q0, q1) a =
  isJust n0 && isJust n1 && Gr.hasEdge (graph a) (fromJust n0, fromJust n1)
  where
    n0 = Map.lookup q0 q
    n1 = Map.lookup q1 q
    q = etats a

stateToNode :: Ord state => state -> Map.Map state Int -> Int
stateToNode n q = fromJust $ Map.lookup n q

removeState ::
     Ord state => state -> NFA state transition -> Maybe (NFA state transition)
removeState st (NFA s q i f g n) =
  if stateExist st (NFA s q i f g n)
    then let q' = Map.delete st q
             i' = Set.delete (stateToNode st q) i
             f' = Set.delete (stateToNode st q) f
             g' = Gr.delNode (stateToNode st q) g
          in pure $ NFA s q' i' f' g' n
    else Nothing

removeTransition ::
     (Ord state, Eq transition)
  => (state, state, transition)
  -> NFA state transition
  -> Maybe (NFA state transition)
removeTransition (q0, q1, t) (NFA s q i f g n) =
  case transformTuple (Map.lookup q0 q, Map.lookup q1 q, t) of
    Just (n0, n1, trans) ->
      let g' = Gr.delLEdge (n0, n1, trans) g
       in pure $ NFA s q i f g' n
    Nothing -> Nothing

removeTransitions ::
     (Ord state, Eq transition)
  => (state, state)
  -> NFA state transition
  -> Maybe (NFA state transition)
removeTransitions (q0, q1) a =
  if stateExist q0 a && stateExist q1 a
    then pure
           $ foldl
               (\acc t ->
                  case removeTransition (q0, q1, t) acc of
                    Just a' -> a'
                    _       -> acc)
               a
           $ sigma a
    else Nothing

makeInit ::
     Ord state => state -> NFA state transition -> Maybe (NFA state transition)
makeInit st (NFA s q i f g n) =
  if stateExist st (NFA s q i f g n)
    then pure $ NFA s q (Set.insert (stateToNode st q) i) f g n
    else Nothing

makeFinal ::
     Ord state => state -> NFA state transition -> Maybe (NFA state transition)
makeFinal st (NFA s q i f g n) =
  if stateExist st (NFA s q i f g n)
    then pure $ NFA s q i (Set.insert (stateToNode st q) f) g n
    else Nothing

isStandard :: NFA state transition -> Bool
isStandard (NFA _ _ i _ g _) = hasOneElem && null (Gr.pre g p)
  where
    i' = Set.toList i
    p = head i'
    hasOneElem =
      case i' of
        []  -> False
        [_] -> True
        _   -> False

isHomogeneous :: (Ord state, Eq transition) => NFA state transition -> Bool
isHomogeneous (NFA _ q _ _ g _) = foldl f True $ Map.keys q
  where
    f acc n = acc && allSameB l
      where
        l = Gr.lpre g $ fromJust $ Map.lookup n q
    allSameB :: Eq b => [(a, b)] -> Bool
    allSameB []          = True
    allSameB ((_, b):xs) = all (\(_, b') -> b == b') xs

makeStandard ::
     (Enum state, Ord state) => NFA state transition -> NFA state transition
makeStandard (NFA s q i f g nu) =
  let q' = Map.insert (toEnum $ nu + 1) (nu + 1) q
      i' = Set.singleton $ nu + 1
      gi = Gr.insNode (nu + 1, (toEnum $ nu + 1)) g
      fun acc n =
        foldl (\acc' (n', a) -> Gr.insEdge (nu + 1, n', a) acc') acc
          $ Gr.lsuc g n
      g' = foldl fun gi i
   in NFA s q' i' f g' $ nu + 1

directSucc :: Ord state => state -> NFA state transition -> Set.Set state
directSucc st (NFA _ q _ _ g _) = Set.fromList $ map (fromJust . Gr.lab g) l
  where
    l = Gr.suc g $ fromJust $ Map.lookup st q

directPred :: Ord state => state -> NFA state transition -> Set.Set state
directPred st (NFA _ q _ _ g _) = Set.fromList $ map (fromJust . Gr.lab g) l
  where
    l = Gr.pre g $ fromJust $ Map.lookup st q

extractListStateAutomata ::
     Ord state
  => Set.Set state
  -> NFA state transition
  -> Maybe (NFA state transition)
extractListStateAutomata o (NFA s q i f g n) =
  if all (`stateExist` a) (Set.toList o)
    then let q' = Map.filterWithKey (\k _ -> k `Set.member` o) q
             i' = Set.intersection i o'
             f' = Set.intersection f o'
             g' = Gr.nfilter (\x -> Set.member x o') g
          in Just (NFA s q' i' f' g' n)
    else Nothing
  where
    a = NFA s q i f g n
    o' = Set.map (\x -> stateToNode x q) o

maximalOrbits :: Ord state => NFA state transition -> [Set.Set state]
maximalOrbits a = map (\l -> Set.fromList $ map (fromJust . Gr.lab g') l) orbitM
  where
    g' = graph a
    sccs = scc g'
    filterFun []     = False
    filterFun (x:[]) = Gr.hasEdge g' (x, x)
    filterFun _      = True
    orbitM = filter filterFun sccs

isOrbit :: Ord state => Set.Set state -> NFA state transition -> Bool
isOrbit o a = isJust g' && firstTest l && hasOneElem fc
  where
    l = map (\x -> fromJust $ Map.lookup x $ etats a) $ Set.toList o
    firstTest []  = False
    firstTest [x] = Gr.hasEdge (graph a) (x, x)
    firstTest _   = True
    hasOneElem [_] = True
    hasOneElem _   = False
    a' = extractListStateAutomata o a
    g' = graph <$> a'
    fc = fromJust $ scc <$> g'

orbitIn ::
     (Ord state, Ord transition)
  => Set.Set state
  -> NFA state transition
  -> Set.Set state
orbitIn o a =
  if isOrbit o a
    then Set.map (\(x, _) -> fromJust $ Gr.lab (graph a) x)
           $ foldl f Set.empty o'
    else Set.empty
  where
    f acc n =
      Set.union acc
        $ Set.filter (\(n', _) -> Set.notMember n' o')
        $ Set.fromList
        $ Gr.lpre (graph a) n
    o' = Set.map (\x -> stateToNode x $ etats a) o

orbitOut ::
     (Ord state, Ord transition)
  => Set.Set state
  -> NFA state transition
  -> Set.Set state
orbitOut o a =
  if isOrbit o a
    then Set.map (\(x, _) -> fromJust $ Gr.lab (graph a) x)
           $ foldl f Set.empty o'
    else Set.empty
  where
    f acc n =
      Set.union acc
        $ Set.filter (\(n', _) -> Set.notMember n' o')
        $ Set.fromList
        $ Gr.lsuc (graph a) n
    o' = Set.map (\x -> stateToNode x $ etats a) o

isStableOrbit ::
     (Ord state, Ord transition)
  => Set.Set state
  -> NFA state transition
  -> Bool
isStableOrbit o a =
  isOrbit o a && inOut == filter (\(x, x') -> hasEdge (x, x') a) inOut
  where
    inO = orbitIn o a
    outO = orbitOut o a
    inOut = do
      x <- Set.toList outO
      y <- Set.toList inO
      return (x, y)

isStronglyStableOrbit ::
     (Ord state, Ord transition, Show state, Show transition)
  => Set.Set state
  -> NFA state transition
  -> Bool
isStronglyStableOrbit o a =
  isOrbit o a
    && if not $ isStableOrbit o a
         then False
         else if maximalOrbits a' == maximalOrbits a
                then True
                else foldl (\acc o' -> acc && isStronglyStableOrbit o' a') True
                       $ maximalOrbits a'
  where
    autoOrbit = fromJust $ extractListStateAutomata o a
    inO = trace (show (orbitIn o a)) $ orbitIn o a
    outO = trace (show (orbitOut o a)) $ orbitOut o a
    outIn = do
      x <- Set.toList outO
      y <- Set.toList inO
      return (x, y)
    a' =
      foldl
        (\n (x, x') ->
           case removeTransitions (x, x') n of
             Just auto -> auto
             _         -> n)
        autoOrbit
        outIn

isTransversOrbit ::
     (Ord state, Ord transition)
  => Set.Set state
  -> NFA state transition
  -> Bool
isTransversOrbit o a = isOrbit o a && Set.size sIn <= 1 && Set.size sOut <= 1
  where
    oIn = orbitIn o a
    oOut = orbitOut o a
    sIn = Set.map (\x -> Set.difference (directPred x a) o) oIn
    sOut = Set.map (\x -> Set.difference (directSucc x a) o) oOut

isStronglyTransversOrbit ::
     (Ord state, Ord transition, Show state, Show transition)
  => Set.Set state
  -> NFA state transition
  -> Bool
isStronglyTransversOrbit o a =
  isOrbit o a
    && if not $ isTransversOrbit o a
         then False
         else if maximalOrbits a' == maximalOrbits a
                then True
                else foldl (\acc o' -> acc && isStronglyStableOrbit o' a') True
                       $ maximalOrbits a'
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
             Just auto -> auto
             _         -> n)
        autoOrbit
        outIn

accept ::
     forall state transition. (Ord state, Eq transition)
  => [transition]
  -> NFA state transition
  -> Bool
accept [] _ = True
accept transitions a = any ((verifyPath transitions)) startNodes
  where
    gr = graph a
    startNodes =
      [ n
      | (s, n) <- Map.toList (etats a)
      , Set.member (stateToNode s $ etats a) $ premier a
      ]
    finalNodes =
      [ n
      | (s, n) <- Map.toList (etats a)
      , Set.member (stateToNode s $ etats a) $ final a
      ]
    verifyPath :: [transition] -> Gr.Node -> Bool
    verifyPath [] currentNode = currentNode `elem` finalNodes
    verifyPath (t:ts) currentNode =
      case findNextNode currentNode t of
        Just nextNode -> verifyPath ts nextNode
        Nothing       -> False
    findNextNode :: Gr.Node -> transition -> Maybe Gr.Node
    findNextNode node trans =
      case [n | (_, n, tr) <- Gr.out gr node, tr == trans] of
        [n] -> Just n
        _   -> Nothing

-- ==================== Affichage =====================
cleanLabel :: TL.Text -> TL.Text
cleanLabel = TL.filter (\c -> c /= '\'' && c /= '\"')

shapeOf :: Ord state => state -> NFA state transition -> Shape
shapeOf lbl a =
  if isFinal lbl a
    then DoubleCircle
    else Circle

colorOf :: Ord state => state -> NFA state transition -> X11Color
colorOf lbl a =
  if isStart lbl a
    then Green
    else White

automatonToDot ::
     (Show transition, Show state, Ord state)
  => NFA state transition
  -> DotGraph Gr.Node
automatonToDot a = graphToDot params $ graph a
  where
    params =
      nonClusteredParams
        { globalAttributes = [GraphAttrs [RankDir FromLeft]]
        , fmtNode =
            \(_, lbl) ->
              [ Shape $ shapeOf lbl a
              , FillColor [toWColor $ colorOf lbl a]
              , Style [SItem Filled []]
              , Label $ StrLabel $ cleanLabel $ TL.pack (show lbl)
              ]
        , fmtEdge =
            \(_, _, l) -> [Label $ StrLabel $ cleanLabel $ TL.pack (show l)]
        }

automatonToDotClustered ::
     forall state transition. (Show transition, Show state, Ord state)
  => [Set.Set state]
  -> NFA state transition
  -> DotGraph Gr.Node
automatonToDotClustered clusters a = graphToDot params $ graph a
  where
    params =
      defaultParams
        { globalAttributes = [GraphAttrs [RankDir FromLeft]]
        , fmtNode =
            \(_, lbl) ->
              [ Shape $ shapeOf lbl a
              , FillColor [toWColor $ colorOf lbl a]
              , Style [SItem Filled []]
              , Label $ StrLabel $ cleanLabel $ TL.pack (show lbl)
              ]
        , fmtEdge =
            \(_, _, l) -> [Label $ StrLabel $ cleanLabel $ TL.pack (show l)]
        , clusterBy = clusterNodes
        , clusterID = Num . Int
        }
    clusterNodes (n, l) =
      case L.findIndex (Set.member l) clusters of
        Just cid -> C cid $ N (n, l)
        Nothing  -> N (n, l)
