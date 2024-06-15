module Prop.PropGlushkov
  ( AExp
  , propGlushkov
  ) where

import qualified Data.Text       as T
import qualified Exp             as E
import qualified GetExp          as GE
import           NFA
import qualified NFAG            as NG
import           Test.QuickCheck

symbols :: [T.Text]
symbols = ["Epsilon", "$", "+", ".", "*", "a", "b", "c", "(", ")"]

newtype TextExp = TextExp
  { getText :: T.Text
  }

newtype AExp = AExp
  { getExp :: E.Exp Char}
  deriving Show

instance Arbitrary TextExp where
  arbitrary = do
    n <- choose (1, 30)
    txts <- vectorOf n (elements symbols)
    return $ TextExp $ T.concat txts

instance Arbitrary AExp where
  arbitrary = do
    txt <- arbitrary :: Gen TextExp
    let maybeExp = GE.expFromText (getText txt)
    case maybeExp of
      Just e -> return $ AExp e
      Nothing  -> arbitrary

propGlushkov :: AExp -> Bool
propGlushkov (AExp e) = isStandard a && allStrongStable && allStrongTransverse
  where
    a = E.glushkov e :: NG.NFAG Int Char
    maxOrbits = maximalOrbits a
    allStrongStable = all (`isStronglyStableOrbit` a) maxOrbits
    allStrongTransverse = all (`isStronglyTransversOrbit` a) maxOrbits
