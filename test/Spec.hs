import qualified NFAF              as NF
import qualified NFAG              as NG
import           Prop.PropGlushkov
import           Prop.PropNFA
import           Test.QuickCheck

main :: IO ()
main = do
  putStrLn "\nTesting NG.NFAG implementation..."
  quickCheck $ flip (propAddState :: Int -> NG.NFAG Int Char -> Bool) . ag
  quickCheck
    $ flip (propAddTransition :: (Int, Int, Char) -> NG.NFAG Int Char -> Bool)
        . ag
  quickCheck $ flip (propRemoveState :: Int -> NG.NFAG Int Char -> Bool) . ag
  quickCheck
    $ flip
        (propRemoveTransition :: (Int, Int, Char) -> NG.NFAG Int Char -> Bool)
        . ag
  quickCheck $ flip (propDirectSucc :: Int -> NG.NFAG Int Char -> Bool) . ag
  quickCheck $ flip (propDirectPred :: Int -> NG.NFAG Int Char -> Bool) . ag
  quickCheck $ (propStandard :: NG.NFAG Int Char -> Bool) . ag
  -- quickCheck $ (propHomogenous :: NG.NFAG Int Char -> Bool) . ag
  putStrLn "Testing NF.NFAF implementation..."
  quickCheck $ flip (propAddState :: Int -> NF.NFAF Int Char -> Bool) . af
  quickCheck
    $ flip (propAddTransition :: (Int, Int, Char) -> NF.NFAF Int Char -> Bool)
        . af
  quickCheck $ flip (propRemoveState :: Int -> NF.NFAF Int Char -> Bool) . af
  quickCheck
    $ flip
        (propRemoveTransition :: (Int, Int, Char) -> NF.NFAF Int Char -> Bool)
        . af
  quickCheck $ flip (propDirectSucc :: Int -> NF.NFAF Int Char -> Bool) . af
  quickCheck $ flip (propDirectPred :: Int -> NF.NFAF Int Char -> Bool) . af
  quickCheck $ (propStandard :: NF.NFAF Int Char -> Bool) . af
  putStrLn "\nTesting Gluskov properties..."
  quickCheck propGlushkov
