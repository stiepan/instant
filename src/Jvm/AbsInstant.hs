module Jvm.AbsInstant where

import qualified AbsGrammar as A

type Ident = A.Ident

data Program = Prog [Stmt]
  deriving (Eq, Ord, Show, Read)

data Stmt = SAss Ident Exp | SExp Exp
  deriving (Eq, Ord, Show, Read)

data Exp
    = ExpAdd {stack :: Integer, expl :: Exp, expr :: Exp}
    | ExpSub {stack :: Integer, expl :: Exp, expr :: Exp}
    | ExpMul {stack :: Integer, expl :: Exp, expr :: Exp}
    | ExpDiv {stack :: Integer, expl :: Exp, expr :: Exp}
    | ExpLit {stack :: Integer, explit :: Integer}
    | ExpVar {stack :: Integer, expvar :: Ident}
  deriving (Eq, Ord, Show, Read)


toAbsProgram :: A.Program -> Program
toAbsProgram (A.Prog stmts) = Prog $ map toAbsStmt stmts


toAbsStmt :: A.Stmt -> Stmt
toAbsStmt (A.SAss var e) = SAss var (toAbsExp e)

toAbsStmt (A.SExp e) = SExp (toAbsExp e)


toAbsExp :: A.Exp -> Exp
toAbsExp (A.ExpLit n) = ExpLit 1 n

toAbsExp (A.ExpVar var) = ExpVar 1 var

toAbsExp (A.ExpAdd e1 e2) = toExpBinary ExpAdd e1 e2

toAbsExp (A.ExpMul e1 e2) = toExpBinary ExpMul e1 e2

toAbsExp (A.ExpDiv e1 e2) = toExpBinary ExpDiv e1 e2

toAbsExp (A.ExpSub e1 e2) = toExpBinary ExpSub e1 e2


toExpBinary c le re = c stackReq sle sre
  where
  sle = toAbsExp le
  sre = toAbsExp re
  lExpStack = stack sle
  rExpStack = stack sre
  stackUp = if lExpStack == rExpStack then 1 else 0
  stackReq = (max lExpStack rExpStack) + stackUp