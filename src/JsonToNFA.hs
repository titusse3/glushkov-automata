module JsonToNFA
  ( parseNFA
  ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Set             as Set
import qualified Data.Map             as Map
import qualified Data.Graph.Inductive as Gr
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
buildNFA (NFAJson n p f ts) =
  let
    sigma' = Set.fromList $ map (\(_, _, t) -> t) ts
    etatsList = zip n [0..]
    etats' = Map.fromList etatsList
    premier' = Set.fromList [idx | s <- p, Just idx <- [Map.lookup s etats']]
    final' = Set.fromList [idx | s <- f, Just idx <- [Map.lookup s etats']]
    edges = [(Map.findWithDefault (-1) s1 etats', Map.findWithDefault (-1) s2 etats', t) | (s1, s2, t) <- ts]
    graph' = Gr.mkGraph (map (\(s, idx) -> (idx, s)) etatsList) edges
  in
    N.NFA
      { N.sigma = sigma'
      , N.etats = etats'
      , N.premier = premier'
      , N.final = final'
      , N.graph = graph'
      , N.lastN = length n
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
