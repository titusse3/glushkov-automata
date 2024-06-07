{
module ExpLex (Token(..), alexScanTokens) where

import qualified Data.Text as T
}

%wrapper "strict-text"

$alpha = [a-zA-Z]

tokens :-

  $white+          ;
  "Epsilon" | "$"  { \_ -> Epsilon }
  "+"              { \_ -> Or }
  "."              { \_ -> And }
  "*"              { \_ -> Star }
  $alpha           { \s -> Sym (T.head s) }
  "("              {\_ -> Pd}
  ")"              {\_ -> Pg}
  .                {\_ -> Error}

{
data Token 
  = Epsilon 
  | And 
  | Or 
  | Star 
  | Sym Char
  | Pd
  | Pg
  | Error
  deriving (Show, Eq)
}
