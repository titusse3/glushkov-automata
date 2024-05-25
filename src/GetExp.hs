module GetExp
  ( fromText
  ) where

import qualified Data.Text as T
import qualified Exp       as E
import qualified ExpHp     as H
import qualified ExpLex    as A

fromText :: T.Text -> Maybe (E.Exp Char)
fromText = H.parseExp . A.alexScanTokens
