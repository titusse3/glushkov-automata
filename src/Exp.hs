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
import qualified Data.Map                 as Map
import           Data.Maybe               (fromJust)
import qualified Data.Set                 as Set
import           NFA                      (NFA (..))

data Exp a
  = Epsilon
  | Star (Exp a)
  | Plus (Exp a) (Exp a)
  | Point (Exp a) (Exp a)
  | Sym a
  deriving (Foldable, Functor, Traversable)

instance (Show a) => Show (Exp a) where
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
isNull Epsilon      = True
isNull (Sym _)      = False
isNull (Plus e e')  = isNull e || isNull e'
isNull (Star _)     = True
isNull (Point e e') = isNull e && isNull e'

firstE :: Exp (a, Int) -> Set.Set Int
firstE Epsilon = Set.empty
firstE (Sym (_, n)) = Set.singleton n
firstE (Plus e e') = Set.union (firstE e) (firstE e')
firstE (Star e) = firstE e
firstE (Point e e') =
  if isNull e
    then Set.union (firstE e) (firstE e')
    else firstE e

lastE :: Exp (a, Int) -> Set.Set Int
lastE Epsilon = Set.empty
lastE (Sym (_, n)) = Set.singleton n
lastE (Plus e e') = Set.union (lastE e) (lastE e')
lastE (Star e) = lastE e
lastE (Point e e') =
  if isNull e'
    then Set.union (lastE e) (lastE e')
    else lastE e'

posE :: Ord a => Exp (a, Int) -> Map.Map Int a
posE Epsilon      = Map.empty
posE (Star e)     = posE e
posE (Plus e e')  = Map.union (posE e) (posE e')
posE (Point e e') = Map.union (posE e) (posE e')
posE (Sym (a, n)) = Map.singleton n a

followE :: Ord a => Exp (a, Int) -> (Int -> Set.Set Int)
followE Epsilon _ = Set.empty
followE (Sym _) _ = Set.empty
followE (Plus e e') s
  | Map.member s (posE e) = followE e s
  | Map.member s (posE e') = followE e' s
  | otherwise = Set.empty
followE (Star e) s =
  if Set.member s (lastE e)
    then Set.union (firstE e) (followE e s)
    else followE e s
followE (Point e e') s
  | Map.member s (posE e') = followE e' s
  | Map.member s (posE e) =
    if Set.member s (lastE e)
      then Set.union f (firstE e')
      else f
  | otherwise = Set.empty
  where
    f = followE e s

glushkov :: Ord a => Exp a -> NFA Int a
glushkov e = NFA sigma' q i f trans
  where
    sigma' = alphabet e
    i = Set.singleton 0
    f =
      if isNull linear
        then Set.insert 0 l
        else l
    linear = linearisation e
    l = lastE linear
    allStates = posE linear
    q = Set.union i $ Set.fromList $ Map.keys allStates
    fist = firstE linear
    trans s t = z
      where
        sFollow =
          if s == 0
            then fist
            else followE linear s
        z = Set.map fst $ Set.filter (\(_, a) -> a == t) nexts
        nexts = Set.map (\n -> (n, fromJust $ Map.lookup n allStates)) sFollow
