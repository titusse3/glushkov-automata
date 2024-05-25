module GetNFA
  ( fromJson
  ) where

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as B
import qualified JsonToNFA            as JNFA
import qualified NFA                  as N

fromJson ::
     ( Ord state
     , Ord transition
     , Aeson.FromJSON state
     , Aeson.FromJSON transition
     )
  => FilePath
  -> IO (Maybe (N.NFA state transition))
fromJson f = do
  jsonStr <- B.readFile f
  let c = JNFA.parseNFA jsonStr
  return c
