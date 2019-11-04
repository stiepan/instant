module Jvm.Grammar where

import Jvm.AbsInstant

data Program = Class String String Modifier [Member] deriving (Show, Eq)
data Member = Method Modifier Procedure deriving (Show, Eq)
data Modifier = Public deriving (Show, Eq)
data Signature = Sig {
  procInv :: Invocation,
  procName :: String,
  procRT :: Type,
  procArgsT :: [Type]
} deriving (Show, Eq)
data Procedure = Proc {
  procSig :: Signature,
  procStack :: (Maybe Stack),
  procLocals :: (Maybe Locals),
  procBody :: [Instr]
} deriving (Show, Eq)
data Stack = Stack Integer deriving (Show, Eq)
data Locals = Locals Integer deriving (Show, Eq)
data Invocation = Virtual | Static | Special deriving (Show, Eq)
data Type = TVoid | TInteger | TBoolean | TClass String | TArray Type deriving (Show, Eq)
data Instr = Push Integer | IAdd | ISub | IMul | IDiv | ILoad Integer | IStore Integer | ALoad Integer |
             Swap | Dup | Pop | GetStatic String Type | IReturn | VReturn | Call Signature deriving (Show, Eq)
