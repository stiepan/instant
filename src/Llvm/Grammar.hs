module Llvm.Grammar where

data Program = Prog [TopLevel] deriving (Show, Eq)
data TopLevel = PDef Procedure | PDecl Signature deriving (Show, Eq)
data Signature = Sig {
  name :: String,
  args :: [Type],
  ret :: Type
} deriving (Show, Eq)
data Procedure = Proc {
  sig :: Signature,
  body :: [Instr]
} deriving (Show, Eq)
data BasicType = TI32 | TVoid deriving (Show, Eq)
data Type = SType BasicType | Pointer BasicType deriving (Show, Eq)
data Register = Reg Type String deriving (Show, Eq)
data Value = VReg Register | VInt Type Integer deriving (Show, Eq)
data Instr = Return Value | Alloca Register | Add Register Value Value | Sub Register Value Value |
             Mul Register Value Value | Div Register Value Value | Store Value Register |
             Load Register Register | Call Signature [Value] deriving (Show, Eq)

sTI32 = SType TI32
pTI32 = Pointer TI32
sTVoid = SType TVoid