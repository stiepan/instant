module Jvm.Compile where

import Jvm.Grammar
import qualified AbsGrammar as Instant

stdInitializer :: Member
stdInitializer = Method Public (Proc initializerSig Nothing Nothing instructions)
  where
  initializerSig = Sig Virtual "<init>" TVoid []
  instructions = [ALoad 0, Call objInit, Return]
  objInit = Sig Virtual "java/lang/Object/<init>" TVoid []


mainMethod :: Stack -> Locals -> [Instr] -> Member
mainMethod (Stack n) ls instrs =
  Method Public (Proc mainSig stack (Just ls) (getStream:instrs))
  where
  stack = Just (Stack (n + 1))
  mainSig = Sig Static "main" TVoid [(TArray (TClass "java/lang/String"))]
  getStream = GetStatic "java/lang/System/out" (TClass "java/io/PrintStream")


compile :: Instant.Program -> Program
compile _ =
  Class Public [stdInitializer, mainMethod s ls instrs]
  where
  s = (Stack 0)
  ls = (Locals 1)
  instrs = [Return]