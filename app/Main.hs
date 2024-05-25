module Main
  ( main
  ) where

import           Control.Monad          (void)
import           Data.GraphViz.Commands (GraphvizOutput (Png), addExtension,
                                         runGraphviz)
import           Data.Maybe             (fromJust)
import qualified Exp                    as E
import           GetExp                 (fromText)
-- import           GetNFA                 (fromJson)
import qualified NFA                    as N

main :: IO ()
main = do
  let t = "(a+b).a*.b*.(a+b)*"
  let expT = fromJust $ fromText t
  let z' = E.glushkov expT
  -- z <- fromJson "automaton1.json"
  -- let z' = fromJust z :: N.NFA Int T.Text
  -- let o = head $ N.maximalOrbit z'
  -- print $ N.isTransversOrbit z' $ Set.fromList [4, 5, 6, 7]
  void $ addExtension (runGraphviz (N.automatonToDot z')) Png "testPNG"
