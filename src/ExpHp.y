{
module ExpHp(parseExp) where

import Control.Applicative (liftA2, liftA)
import qualified Exp as E
import qualified ExpLex as A
import Data.Text as T
import Control.Monad

import Data.Maybe
import Control.Monad.State.Lazy
}

%name parseExp Exp
%tokentype { A.Token }
%error { parseError }
%monad { Maybe }

%token
  epsilon   { A.Epsilon }
  or        { A.Or }
  and       { A.And }
  star      { A.Star }
  sym       { A.Sym $$ }
  pd        { A.Pd }
  pg        { A.Pg }

%left or and
%left star

%%

Exp : pd Exp pg { $2 }
    | epsilon  { E.Epsilon }
    | Exp or Exp { E.Plus $1 $3 }
    | Exp and Exp { E.Point $1 $3 }
    | Exp star { E.Star $1 }
    | sym { (E.Sym $1) }
{

parseError _ = Nothing

}