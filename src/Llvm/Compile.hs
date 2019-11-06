module Llvm.Compile where

import qualified Data.Map.Strict as Map
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Functor.Identity
import Llvm.Grammar
import DList
import qualified AbsGrammar as Instant


data Env = Env {
  -- maps variables names to allocated registers
  bindings :: Map.Map Instant.Ident Register,
  regCount :: Integer -- registers count
}

type Instructions = DList Instr

type Compilation = WriterT Instructions (StateT Env Identity)


newReg :: Type -> Env -> (Env, Register)
newReg t (Env bs regC)= (Env bs (regC + 1), Reg t ("r" ++ show regC))


bindVar :: Instant.Ident -> Register -> Env -> Env
bindVar var reg env = Env (Map.insert var reg (bindings env)) (regCount env)


bindVarM :: Instant.Ident -> Register -> Compilation ()
bindVarM var reg = do
  env <- lift get
  lift $ put $ bindVar var reg env


getVarM :: Instant.Ident -> Compilation (Maybe Register)
getVarM var = do
  env <- lift get
  return $ Map.lookup var (bindings env)


newRegM :: Type -> Compilation Register
newRegM t = do
  env <- lift get
  let (nenv, reg) = newReg t env
  lift $ put nenv
  return reg


emit :: Instr -> Compilation ()
emit = tell.singleton


compileP :: Instant.Program -> String -> Program
compileP (Instant.Prog stmts) _ = Prog tlevels
  where
  tlevels = [printIntDecl, mainDef]
  printIntDecl = PDecl (Sig "printInt" [sTI32] sTVoid)
  mainDef = PDef $ Proc (Sig "main" [] sTI32) body
  body = toList $ (compileBlock stmts) `mappend` ret
  ret = singleton $ Return (VInt sTI32 0)


compileBlock :: [Instant.Stmt] -> Instructions
compileBlock ss = instructions
  where
  ((_, instructions), _) = runState (runWriterT $ mapM_ compileS ss) (Env Map.empty 1)


compileS :: Instant.Stmt -> Compilation ()
compileS (Instant.SExp e) = do
  v <- compileE e
  emit $ Call printProc [v]
  where
  printProc = Sig "printInt" [sTI32] sTVoid

compileS (Instant.SAss var e) = do
  v <- compileE e
  mreg <- getVarM var
  case mreg of
    Just reg -> emit $ Store v reg
    Nothing -> do
      reg <- newRegM pTI32
      bindVarM var reg
      emit $ Alloca reg
      emit $ Store v reg


compileE :: Instant.Exp -> Compilation Value
compileE (Instant.ExpLit n) = return $ VInt sTI32 n

compileE (Instant.ExpVar var) = do
  reg <- newRegM sTI32
  mareg <- getVarM var
  case mareg of
    Just areg -> emit $ Load reg areg
  return $ VReg reg

compileE (Instant.ExpAdd el er) = compileBOp Add el er

compileE (Instant.ExpMul el er) = compileBOp Mul el er

compileE (Instant.ExpSub el er) = compileBOp Sub el er

compileE (Instant.ExpDiv el er) = compileBOp Div el er


compileBOp c el er = do
  lv <- compileE el
  rv <- compileE er
  reg <- newRegM sTI32
  emit $ c reg lv rv
  return $ VReg reg
