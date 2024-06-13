module Exp
  ( Exp(..)
  , followE
  , linearisation
  , alphabet
  , firstE
  , lastE
  , glushkov
  ) where

import           Control.Monad.State.Lazy (MonadState (get, put), State,
                                           evalState)
import qualified Data.Graph.Inductive     as Gr
import qualified Data.Map                 as Map
import           Data.Maybe               (fromJust, isNothing)
import qualified Data.Set                 as Set
import           NFA                      (NFA (..))

import           Debug.Trace

data Exp a
  = Empty
  | Epsilon
  | Star (Exp a)
  | Plus (Exp a) (Exp a)
  | Point (Exp a) (Exp a)
  | Sym a
  deriving (Foldable, Functor, Traversable)

instance (Show a) => Show (Exp a) where
  show Empty        = "∅"
  show Epsilon      = "ε"
  show (Star e)     = mconcat ["(", show e, ")*"]
  show (Plus e e')  = mconcat ["(", show e, "+", show e', ")"]
  show (Point e e') = mconcat ["(", show e, ".", show e', ")"]
  show (Sym a)      = show a

linearisation :: Exp a -> Exp (a, Int)
linearisation e = evalState (linearisation' e) 1
  where
    linearisation' :: Exp a -> State Int (Exp (a, Int))
    linearisation' = traverse f
      where
        f x = do
          n <- get
          put $ n + 1
          return (x, n)

alphabet :: Ord a => Exp a -> Set.Set a
alphabet = foldMap Set.singleton

isNull :: Exp a -> Bool
isNull Empty        = False
isNull Epsilon      = True
isNull (Sym _)      = False
isNull (Plus e e')  = isNull e || isNull e'
isNull (Star _)     = True
isNull (Point e e') = isNull e && isNull e'

firstE :: Ord a => Exp a -> Set.Set a
firstE Empty = Set.empty
firstE Epsilon = Set.empty
firstE (Sym a) = Set.singleton a
firstE (Plus e e') = Set.union (firstE e) (firstE e')
firstE (Star e) = firstE e
firstE (Point e e') =
  if isNull e
    then Set.union (firstE e) (firstE e')
    else firstE e

lastE :: Ord a => Exp a -> Set.Set a
lastE Empty = Set.empty
lastE Epsilon = Set.empty
lastE (Sym a) = Set.singleton a
lastE (Plus e e') = Set.union (lastE e) (lastE e')
lastE (Star e) = lastE e
lastE (Point e e') =
  if isNull e'
    then Set.union (lastE e) (lastE e')
    else lastE e'

indexE :: Exp (a, Int) -> Map.Map Int a
indexE = foldMap (\(a, n) -> Map.singleton n a)

followE :: Ord a => Exp (a, Int) -> Map.Map Int (Set.Set (a, Int))
followE Epsilon = Map.empty
followE (Star e) =
  foldl (\m k -> Map.insertWith Set.union k (firstE e) m) (followE e)
    $ Set.map snd
    $ lastE e
followE (Plus e e') = Map.union (followE e) (followE e')
followE (Point e e') =
  foldl (\m k -> Map.insertWith Set.union k (firstE e') m) uni
    $ Set.map snd
    $ lastE e
  where
    uni = Map.union (followE e) (followE e')
followE (Sym _) = Map.empty
followE Empty = Map.empty

glushkov :: (Ord a, Enum a) => Exp a -> NFA.NFA Int a
glushkov e = NFA.NFA sigma' etat i f' graphLien $ Map.size etat
  where
    sigma' = alphabet e
    linear = linearisation e
    ind = Map.insert 0 (toEnum 0) $ indexE linear
    etat = Map.fromList [(k, k) | k <- Map.keys ind]
    i = Set.singleton 0
    f = Set.map snd $ lastE linear
    f' =
      if isNull linear
        then Set.union f i
        else f
    debI = foldl (\acc n -> Set.insert n acc) Set.empty $ firstE linear
    follow' =
      Map.union (Map.insert 0 debI $ Map.empty)
        $ followE linear
    et = 0 : (Map.keys $ indexE linear)
    graphNoeud = foldl (\acc n -> Gr.insNode (n, n) acc) Gr.empty et
    graphLien =
      foldl
        (\acc n ->
           foldl (\acc' (a, n') -> Gr.insEdge (n, n', a) acc') acc
             $ if isNothing $ Map.lookup n follow'
                 then Set.empty
                 else fromJust $ Map.lookup n follow')
        graphNoeud
        et
