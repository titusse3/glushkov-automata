module GetExp
  ( expFromText
  ) where

import qualified Data.Text as T
import qualified Exp       as E
import qualified ExpHp     as H
import qualified ExpLex    as A

expFromText :: T.Text -> Maybe (E.Exp Char)
expFromText = H.parseExp . A.alexScanTokens
