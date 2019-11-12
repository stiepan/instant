module Jvm.AbsInstant where

import qualified Data.Map.Strict as Map
import qualified AbsGrammar as A
import qualified Jvm.OptimizeAbs as O

type Ident = A.Ident

data Program = Prog [Stmt]
  deriving (Eq, Ord, Show, Read)

data Stmt = SAss Ident Exp | SExp Exp
  deriving (Eq, Ord, Show, Read)

data Exp
    = ExpAdd {swap :: Bool, expl :: Exp, expr :: Exp}
    | ExpSub {swap :: Bool, expl :: Exp, expr :: Exp}
    | ExpMul {swap :: Bool, expl :: Exp, expr :: Exp}
    | ExpDiv {swap :: Bool, expl :: Exp, expr :: Exp}
    | ExpLit {explit :: Integer}
    | ExpVar {expvar :: Ident}
  deriving (Eq, Ord, Show, Read)


optimizeProgram :: A.Program -> Program
optimizeProgram (A.Prog stmts) = Prog $ map (optimizeStmt stackReq) stmts
  where
  stackReq = O.minStack (map getExp stmts)
  getExp (A.SAss _ e) = e
  getExp (A.SExp e) = e


optimizeStmt :: Integer -> A.Stmt -> Stmt
optimizeStmt h (A.SAss var e) = SAss var (optimizeExp h e)

optimizeStmt h (A.SExp e) = SExp (optimizeExp h e)


optimizeExp :: Integer -> A.Exp -> Exp
optimizeExp h e = reconstruct m h he
  where
  (_, m, he) = O.swapsOptimized h e


reconstruct :: O.SwapMap -> Integer -> O.HExp -> Exp

reconstruct _ _ (O.HExpLit _ n) = ExpLit n

reconstruct _ _ (O.HExpVar _ var) = ExpVar var

reconstruct m h (O.HExpAdd _ e1 e2) = reconstructBin m 0 h ExpAdd e1 e2

reconstruct m h (O.HExpMul _ e1 e2) = reconstructBin m 0 h ExpMul e1 e2

reconstruct m h (O.HExpDiv _ e1 e2) = reconstructBin m 1 h ExpDiv e1 e2

reconstruct m h (O.HExpSub _ e1 e2) = reconstructBin m 1 h ExpSub e1 e2


reconstructBin :: O.SwapMap -> Integer -> Integer -> (Bool -> Exp -> Exp -> Exp) -> O.HExp -> O.HExp -> Exp
reconstructBin m swapPenalty h c el er =
  if leftFirst <= rightFirst then
    c False (reconstruct m h el) (reconstruct m (h - 1) er)
  else
    c True (reconstruct m (h - 1) el) (reconstruct m h er)
  where
  leftFirstR = optimizeSwaps m (h - 1) er
  rightFirstL = optimizeSwaps m (h - 1) el
  leftFirstL = optimizeSwaps m h el
  rightFirstR = optimizeSwaps m h er
  leftFirst = leftFirstL + leftFirstR
  rightFirst = rightFirstL + rightFirstR + swapPenalty


optimizeSwaps m h e = (m Map.! (O.hashE e)) Map.! h
