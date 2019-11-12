module Jvm.Compile where

import qualified Data.Map.Strict as Map
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Functor.Identity
import Jvm.Grammar
import DList
import qualified AbsGrammar as InstantG
import qualified Jvm.AbsInstant as Instant

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


compileP :: InstantG.Program -> String -> Program
compileP p baseName = compileProgram (Instant.optimizeProgram p) baseName

compileProgram :: Instant.Program -> String -> Program
compileProgram (Instant.Prog stmts) baseName =
  Class baseName "java/lang/Object" Public [stdInitializer, mainMethod]
  where
  (stackSize, localsCount, insts) = compileBlock stmts

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
    stack = Just (Stack stackSize)
    locals = Just (Locals localsCount)
    mainSig = Sig Static "main" TVoid [(TArray (TClass "java/lang/String"))]


compileBlock :: [Instant.Stmt] -> (Integer, Integer, Instructions)
compileBlock ss = (stackSize, (freeLocal env) + 1, instructions)
  where
  ((stackSizes, instructions), env) = runState (runWriterT $ mapM compileS ss) (Env Map.empty 0)
  stackSize = case stackSizes of
    [] -> 0
    ss -> foldr1 max ss


compileS :: Instant.Stmt -> Compilation Integer
compileS (Instant.SExp e) = do
  emit getStatic
  stackSize <- compileE e
  emit $ Call printMethod
  return (stackSize + 1)
  where
  printMethod = Sig Virtual "java/io/PrintStream/println" TVoid [TInteger]
  getStatic = GetStatic "java/lang/System/out" (TClass "java/io/PrintStream")

compileS (Instant.SAss var e) = do
  stackSize <- compileE e
  location <- getOrAllocM var
  emit $ IStore location
  return stackSize


compileE :: Instant.Exp -> Compilation Integer
compileE (Instant.ExpLit n) = do
  emit $ Push n
  return 1

compileE (Instant.ExpVar var) = do
  location <- getM var
  emit $ ILoad location
  return 1

compileE (Instant.ExpAdd s el er) = compileBOp s IAdd el er

compileE (Instant.ExpMul s el er) = compileBOp s IMul el er

compileE (Instant.ExpSub s el er) = compileBOp s ISub el er

compileE (Instant.ExpDiv s el er) = compileBOp s IDiv el er

compileBOp :: Bool -> Instr -> Instant.Exp -> Instant.Exp -> Compilation Integer
compileBOp isSwapped opcode el er =
  if not isSwapped then do
    sl <- compileE el
    sr <- compileE er
    emit opcode
    return $ max sl (sr + 1)
  else do
    sr <- compileE er
    sl <- compileE el
    if opcode `elem` [ISub, IDiv] then do
      emit Swap
      emit opcode
    else do
      emit opcode
    return $ max sr (sl + 1)
