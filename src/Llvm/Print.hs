module Llvm.Print where

import Control.Monad.Trans.Writer
import Data.Functor.Identity
import Jvm.Print (emit, emitSepSs, emitSs, separated, joinedM,)

import Data.Monoid
import DList
import Llvm.Grammar

type Printing = WriterT (DList Char) Identity

showProg :: Program -> String
showProg (Prog ts) = toList $ execWriter $ printProgram ts


printProgram :: [TopLevel] -> Printing ()
printProgram ts = joinedM "\n\n" (map printTLevel ts)


printTLevel :: TopLevel -> Printing ()
printTLevel (PDecl sig) = do
  emit "declare "
  printDSig sig

printTLevel (PDef (Proc sig body)) = do
  emit "define "
  printDSig sig
  emit " {\n"
  mapM_ (\inst -> emit "    " >> printInstr inst >> emit "\n") body
  emit "}"


printDSig :: Signature -> Printing ()
printDSig (Sig name ts rt) = do
  printType rt
  emitSs [" @", name, "("]
  joinedM " " (map printType ts)
  emit ")"


printType :: Type -> Printing ()
printType (SType t) = printBType t

printType (Pointer t) = do
  printBType t
  emit "*"


printBType :: BasicType -> Printing ()
printBType TI32 = emit "i32"

printBType TVoid = emit "void"


printValueTyped :: Value -> Printing ()
printValueTyped (VInt t n) = do
  printType t
  emit " "
  emit $ show n

printValueTyped (VReg r) = do
  printRegTyped r


printRegTyped :: Register -> Printing ()
printRegTyped (Reg t name) = do
  printType t
  emit " %"
  emit $ name


printVal :: Value -> Printing ()
printVal (VInt t n) = do
  emit $ show n

printVal (VReg (Reg t name)) = do
  emit "%"
  emit $ name


printInstr :: Instr -> Printing ()
printInstr (Return v) = do
  emit "ret "
  printValueTyped v

printInstr (Alloca (Reg (Pointer st) name)) = do
  emitSs ["%", name, " = alloca "]
  printType (SType st)

printInstr (Add (Reg t name) lv rv) = printBOp "add" name t lv rv

printInstr (Sub (Reg t name) lv rv) = printBOp "sub" name t lv rv

printInstr (Mul (Reg t name) lv rv) = printBOp "mul" name t lv rv

printInstr (Div (Reg t name) lv rv) = printBOp "sdiv" name t lv rv

printInstr (Store v (Reg t name)) = do
  emit "store "
  printValueTyped v
  emit ", "
  printType t
  emitSs [" %", name]

printInstr (Load (Reg lt lname) rr) = do
  emitSs ["%", lname, " = load "]
  printType lt
  emit ", "
  printRegTyped rr

printInstr (Call (Sig name _ rt) vs) = do
  emit "call "
  printType rt
  emitSs [" @", name, "("]
  joinedM " " (map printValueTyped vs)
  emit ")"


printBOp opcode rname rtype lv rv = do
  emitSs ["%", rname, " = ", opcode, " "]
  printType rtype
  emit " "
  joinedM ", " (map printVal [lv, rv])

--printLocals :: Maybe Locals -> Printing ()
--printLocals Nothing = return ()
--
--printLocals (Just (Locals count)) = do
--  emit ".limit locals "
--  emit $ show count
--  emit "\n"
--
--
--printStack :: Maybe Stack -> Printing ()
--printStack Nothing = return ()
--
--printStack (Just (Stack size)) = do
--  emit ".limit stack "
--  emit $ show size
--  emit "\n"
--
--
--printInst :: Instr -> Printing ()
--printInst IAdd = emit "iadd"
--
--printInst ISub = emit "isub"
--
--printInst IMul = emit "imul"
--
--printInst IDiv = emit "idiv"
--
--printInst Swap = emit "swap"
--
--printInst Dup = emit "dup"
--
--printInst Pop = emit "pop"
--
--printInst IReturn = emit "ireturn"
--
--printInst VReturn = emit "return"
--
--printInst (Push n)
--  | n == -1 = emit "iconst_m1"
--  | 0 <= n && n <= 5 = emitSs ["iconst_", show n]
--  | -128 <= n && n < 128 = emitSs ["bipush ", show n]
--  | -65536 <= n && n < 65536 = emitSs ["sipush ", show n]
--  | otherwise = emitSs ["ldc ", show n]
--
--printInst (ILoad n)
--  | 0 <= n && n <= 3 = emitSs ["iload_", show n]
--  | otherwise = emitSs ["iload ", show n]
--
--printInst (IStore n)
--  | 0 <= n && n <= 3 = emitSs ["istore_", show n]
--  | otherwise = emitSs ["istore ", show n]
--
--printInst (ALoad n)
--  | 0 <= n && n <= 3 = emitSs ["aload_", show n]
--  | otherwise = emitSs ["aload ", show n]
--
--printInst (GetStatic name t) = do
--  emit "getstatic "
--  emit name
--  emit " "
--  printType t
--
--printInst (Call sig) = do
--  emit "invoke"
--  printInvocation $ procInv sig
--  emit " "
--  printSig sig
--
--
--printInvocation :: Invocation -> Printing ()
--printInvocation Virtual = emit "virtual"
--
--printInvocation Static = emit "static"
--
--printInvocation Special = emit "special"
--
--
--isStaticMethod :: Procedure -> Bool
--isStaticMethod proc = procInv (procSig proc) == Static
