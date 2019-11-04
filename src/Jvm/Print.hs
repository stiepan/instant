module Jvm.Print where

import Control.Monad.Trans.Writer
import Data.Functor.Identity

import Data.Monoid
import DList
import Jvm.Grammar

type Printing = WriterT (DList Char) Identity


emit :: String -> Printing ()
emit = tell.toDList


emitSs :: [String] -> Printing ()
emitSs = mapM_ emit


emitSepSs :: String -> [String] -> Printing ()
emitSepSs sep ss = emitSs $ separated sep ss


separated :: a -> [a] -> [a]
separated sep [] = []
separated sep (s:[]) = [s]
separated sep (s:ss) = s:sep:(separated sep ss)


joinedM :: String -> [Printing ()] -> Printing ()
joinedM sep ps = sequence_ (separated (emit sep) ps)


showProg :: Program -> String
showProg program = toList $ execWriter $ printProgram program


printProgram :: Program -> Printing ()
printProgram (Class name super modifier ms) = do
  joinedM "\n" [classH, classS, members]
  where
  classH = do
    emitSs [".class", " "]
    printModifier modifier
    emit " "
    emit name
  classS = emitSs [".super ", super, "\n"]
  members = joinedM "\n\n" (map printMember ms)


printModifier :: Modifier -> Printing ()
printModifier Public = emit "public"


printMember :: Member -> Printing ()
printMember (Method modifier proc) = do
  emit ".method "
  printModifier modifier
  emit " "
  if isStaticMethod proc then emit "static " else return ()
  printProc proc


printProc :: Procedure -> Printing ()
printProc (Proc sig s l is) = do
  printSig sig
  emit "\n"
  printLocals l
  printStack s
  mapM_ (\inst -> emit "    " >> printInst inst >> emit "\n") is
  emit ".end method"
  emit "\n"


printSig :: Signature -> Printing ()
printSig (Sig _ name rt ats) = do
  emit name
  emit "("
  mapM_ printType ats
  emit ")"
  printType rt


printType :: Type -> Printing ()
printType TVoid = emit "V"

printType TInteger = emit "I"

printType TBoolean = emit "Z"

printType (TClass name) = do
  emit "L"
  emit name
  emit ";"

printType (TArray t) = do
  emit "["
  printType t


printLocals :: Maybe Locals -> Printing ()
printLocals Nothing = return ()

printLocals (Just (Locals count)) = do
  emit ".limit locals "
  emit $ show count
  emit "\n"


printStack :: Maybe Stack -> Printing ()
printStack Nothing = return ()

printStack (Just (Stack size)) = do
  emit ".limit stack "
  emit $ show size
  emit "\n"


printInst :: Instr -> Printing ()
printInst IAdd = emit "iadd"

printInst ISub = emit "isub"

printInst IMul = emit "imul"

printInst IDiv = emit "idiv"

printInst Swap = emit "swap"

printInst Dup = emit "dup"

printInst Pop = emit "pop"

printInst IReturn = emit "ireturn"

printInst VReturn = emit "return"

printInst (Push n)
  | n == -1 = emit "iconst_m1"
  | 0 <= n && n <= 5 = emitSs ["iconst_", show n]
  | -128 <= n && n < 128 = emitSs ["bipush ", show n]
  | -65536 <= n && n < 65536 = emitSs ["sipush ", show n]
  | otherwise = emitSs ["ldc ", show n]

printInst (ILoad n)
  | 0 <= n && n <= 3 = emitSs ["iload_", show n]
  | otherwise = emitSs ["iload ", show n]

printInst (IStore n)
  | 0 <= n && n <= 3 = emitSs ["istore_", show n]
  | otherwise = emitSs ["istore ", show n]

printInst (ALoad n)
  | 0 <= n && n <= 3 = emitSs ["aload_", show n]
  | otherwise = emitSs ["aload ", show n]

printInst (GetStatic name t) = do
  emit "getstatic "
  emit name
  emit " "
  printType t

printInst (Call sig) = do
  emit "invoke"
  printInvocation $ procInv sig
  emit " "
  printSig sig


printInvocation :: Invocation -> Printing ()
printInvocation Virtual = emit "virtual"

printInvocation Static = emit "static"

printInvocation Special = emit "special"


isStaticMethod :: Procedure -> Bool
isStaticMethod proc = procInv (procSig proc) == Static
