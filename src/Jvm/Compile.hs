module Jvm.Compile where

import qualified Data.Map.Strict as Map
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Functor.Identity
import Jvm.Grammar
import DList
import qualified AbsGrammar as Instant

data Env = Env {
  -- maps variables to local variables array indices
  bindings :: Map.Map Instant.Ident Integer,
  freeLocal :: Integer -- lowest free location
}

type Instructions = DList Instr

type Compilation = WriterT Instructions (StateT Env Identity)


emit :: Instr -> Compilation ()
emit = tell.singleton


alloc :: Instant.Ident -> Env -> Env
alloc var env = Env (Map.insert var location (bindings env)) (location + 1)
  where
  location = freeLocal env


getOrAlloc :: Instant.Ident -> Env -> (Integer, Env)
getOrAlloc var env =
  case Map.lookup var (bindings env) of
      Just location -> (location, env)
      Nothing -> (freeLocal env, alloc var env)


getOrAllocM :: Instant.Ident -> Compilation Integer
getOrAllocM var = do
  env <- lift get
  let (location, nenv) = getOrAlloc var env
  lift $ put nenv
  return location


getM :: Instant.Ident -> Compilation Integer
getM var = do
  env <- lift get
  case Map.lookup var (bindings env) of Just location -> return location

compileP :: Instant.Program -> Program
compileP (Instant.Prog stmts) =
  Class "Main" "java/lang/Object" Public [stdInitializer, mainMethod]
  where
  (localsCount, insts) = compileBlock stmts

  stdInitializer :: Member
  stdInitializer = Method Public (Proc initializerSig Nothing Nothing instructions)
    where
    initializerSig = Sig Virtual "<init>" TVoid []
    instructions = [ALoad 0, Call objInit, VReturn]
    objInit = Sig Special "java/lang/Object/<init>" TVoid []

  mainMethod :: Member
  mainMethod = Method Public (Proc mainSig stack locals body)
    where
    body = toList $ insts `mappend` (singleton VReturn)
    stack = Just (Stack 10)
    locals = Just (Locals localsCount)
    mainSig = Sig Static "main" TVoid [(TArray (TClass "java/lang/String"))]


compileBlock :: [Instant.Stmt] -> (Integer, Instructions)
compileBlock ss = ((freeLocal env) + 1, instructions)
  where
--  ((_, instructions), env) = runState (runWriterT $ sequence (map compileS ss)) (Env Map.empty 0)
  ((_, instructions), env) = runState (runWriterT $ mapM_ compileS ss) (Env Map.empty 0)


compileS :: Instant.Stmt -> Compilation ()
compileS (Instant.SExp e) = do
  emit getStatic
  compileE e
  emit $ Call printMethod
  where
  printMethod = Sig Virtual "java/io/PrintStream/println" TVoid [TInteger]
  getStatic = GetStatic "java/lang/System/out" (TClass "java/io/PrintStream")

compileS (Instant.SAss var e) = do
  compileE e
  location <- getOrAllocM var
  emit $ IStore location


compileE :: Instant.Exp -> Compilation ()
compileE (Instant.ExpLit n) = emit $ Push n

compileE (Instant.ExpVar var) = do
  location <- getM var
  emit $ ILoad location

compileE (Instant.ExpAdd el er) = compileBOp IAdd el er

compileE (Instant.ExpMul el er) = compileBOp IMul el er

compileE (Instant.ExpSub el er) = compileBOp ISub el er

compileE (Instant.ExpDiv el er) = compileBOp IDiv el er

compileBOp :: Instr -> Instant.Exp -> Instant.Exp -> Compilation ()
compileBOp opcode el er = do
  compileE el
  compileE er
  emit opcode