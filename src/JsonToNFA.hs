module JsonToNFA
  ( parseNFA
  ) where

import qualified NFA                  as N

import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Map             as Map
import qualified Data.Set             as Set

type Transition state transition = (state, state, transition)

data JSONNFA state transition = JSONNFA
  { nodes       :: [state]
  , first       :: [state]
  , final       :: [state]
  , transitions :: [Transition state transition]
  }

instance (Ord state, FromJSON state, FromJSON transition) =>
         FromJSON (JSONNFA state transition) where
  parseJSON =
    withObject "JSONNFA" $ \v ->
      JSONNFA
        <$> v .: "Node"
        <*> v .: "First"
        <*> v .: "Final"
        <*> v .: "Transition"

jsonToNFA ::
     (Ord state, Ord transition)
  => JSONNFA state transition
  -> N.NFA state transition
jsonToNFA (JSONNFA ns fs fl tr) =
  N.NFA
    { N.sigma = Set.fromList $ map (\(_, _, l) -> l) tr
    , N.etats = Set.fromList ns
    , N.premier = Set.fromList fs
    , N.final = Set.fromList fl
    , N.delta = buildDelta tr
    }

buildDelta ::
     (Ord state, Ord transition)
  => [Transition state transition]
  -> (state -> transition -> Set.Set state)
buildDelta trs = \s t -> Map.findWithDefault Set.empty (s, t) m
  where
    m =
      foldl
        (\acc (n, n', l) ->
           Map.insertWith Set.union (n, l) (Set.singleton n') acc)
        Map.empty
        trs

parseNFA ::
     (Ord state, Ord transition, FromJSON state, FromJSON transition)
  => B.ByteString
  -> Maybe (N.NFA state transition)
parseNFA jsonData = do
  jsonNFA <- decode jsonData
  return $ jsonToNFA jsonNFA
