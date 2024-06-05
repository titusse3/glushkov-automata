module JsonToNFA
  ( parseNFA
  ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Set             as Set
import           GHC.Generics
import qualified NFA                  as N

data NFAJson state transition = NFAJson
  { nodes       :: [state]
  , first       :: [state]
  , final       :: [state]
  , transitions :: [(state, state, transition)]
  } deriving (Generic, Show)

instance (Ord state, Ord transition, FromJSON state, FromJSON transition) =>
         FromJSON (NFAJson state transition)

instance (Ord state, Ord transition, ToJSON state, ToJSON transition) =>
         ToJSON (NFAJson state transition)

buildNFA ::
     (Ord state, Ord transition)
  => NFAJson state transition
  -> N.NFA state transition
buildNFA (NFAJson nodes first final transitions) =
  N.NFA
    { N.sigma = Set.fromList $ map (\(_, _, t) -> t) transitions
    , N.etats = Set.fromList nodes
    , N.premier = Set.fromList first
    , N.final = Set.fromList final
    , N.delta =
        \s t ->
          Set.fromList
            [s' | (s1, s2, t') <- transitions, s1 == s, t' == t, let s' = s2]
    }

parseNFA ::
     (Ord state, Ord transition, FromJSON state, FromJSON transition)
  => FilePath
  -> IO (Either String (N.NFA state transition))
parseNFA filePath = do
  jsonData <- B.readFile filePath
  let parsed =
        eitherDecode jsonData :: ( Ord state
                                 , Ord transition
                                 , FromJSON state
                                 , FromJSON transition
                                 ) =>
          Either String (NFAJson state transition)
  return $ fmap buildNFA parsed