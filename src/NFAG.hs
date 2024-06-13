module NFAG
  ( NFAG(..)
  ) where

import qualified Data.Graph.Inductive              as Gr
import           Data.Graph.Inductive.Query.DFS
import           Data.GraphViz
import           Data.GraphViz.Attributes.Complete
import qualified Data.List                         as L
import qualified Data.Map                          as Map
import           Data.Maybe
import qualified Data.Set                          as Set
import qualified Data.Text.Lazy                    as TL
import           NFA

data NFAG state transition = NFAG
  { sigma   :: Set.Set transition
  , etats   :: Map.Map state Int
  , premier :: Set.Set Int
  , final   :: Set.Set Int
  , graph   :: Gr.Gr state transition
  , lastN   :: Int
  }

transformTuple :: (Maybe Int, Maybe Int, a) -> Maybe (Int, Int, a)
transformTuple (m1, m2, a) = do
  v1 <- m1
  v2 <- m2
  return (v1, v2, a)

stateToNode :: Ord state => state -> Map.Map state Int -> Int
stateToNode n q = fromJust $ Map.lookup n q

cleanLabel :: TL.Text -> TL.Text
cleanLabel = TL.filter (\c -> c /= '\'' && c /= '\"')

shapeOf ::
     (Ord state, Ord transition, Show state, Show transition) => state -> NFAG state transition -> Shape
shapeOf lbl a =
  if isFinal lbl a
    then DoubleCircle
    else Circle

colorOf ::
     (Ord state, Ord transition, Show state, Show transition) => state -> NFAG state transition -> X11Color
colorOf lbl a =
  if isStart lbl a
    then Green
    else White

instance (Ord state, Ord transition, Show state, Show transition) => NFA (NFAG state transition) where
  type StateType (NFAG state transition) = state
  type TransitionType (NFAG state transition) = transition

  emptyNFA = (NFAG Set.empty Map.empty Set.empty Set.empty Gr.empty 0)

  addState st (NFAG s q i f g n) =
    if stateExist st (NFAG s q i f g n)
      then Nothing
      else let q' = Map.insert st (n + 1) q
               g' = Gr.insNode (n + 1, st) g
            in pure $ NFAG s q' i f g' (n + 1)

  addTransition (q0, q1, t) (NFAG s q i f g n) =
    case transformTuple (Map.lookup q0 q, Map.lookup q1 q, t) of
      Just (n0, n1, trans) ->
        let g' = Gr.insEdge (n0, n1, trans) g
            s' = Set.insert trans s
         in pure $ NFAG s' q i f g' n
      Nothing -> Nothing

  stateExist st = Map.member st . etats

  isFinal lbl nfa =
    case Map.lookup lbl (etats nfa) of
      Just index -> Set.member index (final nfa)
      Nothing    -> False

  isStart lbl nfa =
    case Map.lookup lbl (etats nfa) of
      Just index -> Set.member index (premier nfa)
      Nothing    -> False

  transitionExist (q0, q1, t) (NFAG _ q _ _ g _) =
    let n0 = Map.lookup q0 q
        n1 = Map.lookup q1 q
        s = Gr.hasLEdge g <$> transformTuple (n0, n1, t)
     in isJust s && fromJust s

  hasEdge (q0, q1) a =
    isJust n0 && isJust n1 && Gr.hasEdge (graph a) (fromJust n0, fromJust n1)
    where
      n0 = Map.lookup q0 q
      n1 = Map.lookup q1 q
      q = etats a

  removeState st (NFAG s q i f g n) =
    if stateExist st (NFAG s q i f g n)
      then let q' = Map.delete st q
               i' = Set.delete (stateToNode st q) i
               f' = Set.delete (stateToNode st q) f
               g' = Gr.delNode (stateToNode st q) g
            in pure $ NFAG s q' i' f' g' n
      else Nothing

  removeTransition (q0, q1, t) (NFAG s q i f g n) =
    case transformTuple (Map.lookup q0 q, Map.lookup q1 q, t) of
      Just (n0, n1, trans) ->
        let g' = Gr.delLEdge (n0, n1, trans) g
         in pure $ NFAG s q i f g' n
      Nothing -> Nothing

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

  makeInit st (NFAG s q i f g n) =
    if stateExist st (NFAG s q i f g n)
      then pure $ NFAG s q (Set.insert (stateToNode st q) i) f g n
      else Nothing

  makeFinal st (NFAG s q i f g n) =
    if stateExist st (NFAG s q i f g n)
      then pure $ NFAG s q i (Set.insert (stateToNode st q) f) g n
      else Nothing

  isStandard (NFAG _ _ i _ g _) = hasOneElem && null (Gr.pre g p)
    where
      i' = Set.toList i
      p = head i'
      hasOneElem =
        case i' of
          []  -> False
          [_] -> True
          _   -> False

  isHomogeneous (NFAG _ q _ _ g _) = foldl f True $ Map.keys q
    where
      f acc n = acc && allSameB l
        where
          l = Gr.lpre g $ fromJust $ Map.lookup n q
      allSameB :: Eq b => [(a, b)] -> Bool
      allSameB []          = True
      allSameB ((_, b):xs) = all (\(_, b') -> b == b') xs

  makeStandard (NFAG s q i f g nu) =
    let q' = Map.insert (toEnum $ nu + 1) (nu + 1) q
        i' = Set.singleton $ nu + 1
        f' =
          if null (Set.difference f i)
            then Set.insert (nu + 1) f
            else f
        gi = Gr.insNode (nu + 1, (toEnum $ nu + 1)) g
        fun acc n =
          foldl (\acc' (n', a) -> Gr.insEdge (nu + 1, n', a) acc') acc
            $ Gr.lsuc g n
        g' = foldl fun gi i
     in NFAG s q' i' f' g' $ nu + 1

  -- makeHomogeneous ::
  --     (Enum state, Ord state, Ord transition) => NFAG state transition -> NFAG state transition
  -- makeHomogeneous a =
  --   foldl homoGene a' $ map (\x -> stateToNode x $ etats a) $ Map.keys $ etats a
  --   where a' = makeStandard a
  --         homoGene auto n =
  --           if null $ lp n then
  --             auto
  --           else
  --             fst $ foldl (\(acc, table) (n', t) ->
  --               case Map.lookup t table of
  --                 Just e -> (NFAG (sigma acc) (etats acc) (premier acc) (final acc) (Gr.insEdge (e, n, t) $ graph acc) (lastN acc), table)
  --                 _ -> (NFAG (sigma acc)
  --                           (Map.insert t (toEnum (lastN acc + 1)) $ etats acc)
  --                           (addSpeEtat n' (toEnum (lastN acc + 1)) $ premier acc)
  --                           (addSpeEtat n' (toEnum (lastN acc + 1)) $ final acc)
  --                           (Gr.insNode (lastN acc + 1, toEnum (lastN acc + 1)) $ graph acc)
  --                           (lastN acc + 1),
  --                           Map.insert t (lastN acc + 1) table))
  --                           (auto, Map.empty) $ lp n
  --           where lp s = Gr.lpre (graph a') s
  --                 addSpeEtat etat newEtat ensemble =
  --                   if Set.member etat ensemble then
  --                     Set.insert newEtat ensemble
  --                   else
  --                     ensemble

  directSucc st (NFAG _ q _ _ g _) = Set.fromList $ map (fromJust . Gr.lab g) l
    where
      l = Gr.suc g $ fromJust $ Map.lookup st q

  directPred st (NFAG _ q _ _ g _) = Set.fromList $ map (fromJust . Gr.lab g) l
    where
      l = Gr.pre g $ fromJust $ Map.lookup st q

  extractListStateAutomata o (NFAG s q i f g n) =
    if all (`stateExist` a) (Set.toList o)
      then let q' = Map.filterWithKey (\k _ -> k `Set.member` o) q
               i' = Set.intersection i o'
               f' = Set.intersection f o'
               g' = Gr.subgraph (map (\x -> stateToNode x q) $ Set.toList o) g
            in Just (NFAG s q' i' f' g' n)
      else Nothing
    where
      a = NFAG s q i f g n
      o' = Set.map (\x -> stateToNode x q) o

  maximalOrbits a =
    map (\l -> Set.fromList $ map (fromJust . Gr.lab g') l) orbitM
    where
      g' = graph a
      sccs = scc g'
      filterFun []     = False
      filterFun (x:[]) = Gr.hasEdge g' (x, x)
      filterFun _      = True
      orbitM = filter filterFun sccs

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

  orbitIn o a =
    if isOrbit o a
      then foldl f Set.empty o
      else Set.empty
    where
      f acc n =
        if isStart n a || not (null l)
          then Set.insert n acc
          else acc
        where
          l =
            Set.filter (\n' -> Set.notMember n' o')
              $ Set.fromList
              $ Gr.pre (graph a)
              $ stateToNode n
              $ etats a
      o' = Set.map (\x -> stateToNode x $ etats a) o

  orbitOut o a =
    if isOrbit o a
      then foldl f Set.empty o
      else Set.empty
    where
      f acc n =
        if isFinal n a || not (null l)
          then Set.insert n acc
          else acc
        where
          l =
            Set.filter (\n' -> Set.notMember n' o')
              $ Set.fromList
              $ Gr.suc (graph a)
              $ stateToNode n
              $ etats a
      o' = Set.map (\x -> stateToNode x $ etats a) o

  isStableOrbit o a =
    isOrbit o a && inOut == filter (\(x, x') -> hasEdge (x, x') a) inOut
    where
      inO = orbitIn o a
      outO = orbitOut o a
      inOut = do
        x <- Set.toList outO
        y <- Set.toList inO
        return (x, y)

  isStronglyStableOrbit s a = 
      isStronglyStableOrbit' s a
    where 
      isStronglyStableOrbit' o a'=
        isOrbit o a'
          && if not $ isStableOrbit o a'
              then False
              else foldl (\acc o' -> acc && isStronglyStableOrbit' o' a'') True
                      $ maximalOrbits a''
        where
          autoOrbit = fromJust $ extractListStateAutomata o a'
          inO = orbitIn o a
          outO = orbitOut o a
          outIn = do
            x <- Set.toList outO
            y <- Set.toList inO
            return (x, y)
          a'' =
            foldl
              (\n (x, x') ->
                case removeTransitions (x, x') n of
                  Just auto -> auto
                  _         -> n)
              autoOrbit
              outIn

  isTransversOrbit o a = isOrbit o a && Set.size sIn <= 1 && Set.size sOut <= 1
    where
      oIn = orbitIn o a
      oOut = orbitOut o a
      sIn = Set.map (\x -> Set.difference (directPred x a) o) oIn
      sOut = Set.map (\x -> Set.difference (directSucc x a) o) oOut

  isStronglyTransversOrbit s a = 
      isStronglyTransversOrbit' s a
    where 
      isStronglyTransversOrbit' o a'=
        isOrbit o a'
          && if not $ isTransversOrbit o a'
              then False
              else foldl 
                (\acc o' -> acc && isStronglyTransversOrbit' o' a'') True
                  $ maximalOrbits a''
        where
          autoOrbit = fromJust $ extractListStateAutomata o a'
          inO = orbitIn o a
          outO = orbitOut o a
          outIn = do
            x <- Set.toList outO
            y <- Set.toList inO
            return (x, y)
          a'' =
            foldl
              (\n (x, x') ->
                case removeTransitions (x, x') n of
                  Just auto -> auto
                  _         -> n)
              autoOrbit
              outIn

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

  automatonToDot a = graphToDot params $ graph a
    where
      params =
        nonClusteredParams
          { globalAttributes = [GraphAttrs [RankDir FromLeft, 
            BgColor [toWC $ RGBA 0 0 0 0]]]
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
          
  automatonToDotClustered clusters a = graphToDot params $ graph a
    where
      params =
        defaultParams
          { globalAttributes = [GraphAttrs [RankDir FromLeft, 
            BgColor [toWC $ RGBA 0 0 0 0]]]
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
