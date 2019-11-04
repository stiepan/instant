module Jvm.Grammar where

import Jvm.AbsInstant

data Program = Class String String Modifier [Member] deriving Show
data Member = Method Modifier Procedure deriving Show
data Modifier = Public deriving Show
data Signature = Sig Invocation String Type [Type] deriving Show
data Procedure = Proc Signature (Maybe Stack) (Maybe Locals) [Instr] deriving Show
data Stack = Stack Integer deriving Show
data Locals = Locals Integer deriving Show
data Invocation = Virtual | Static | Special deriving Show
data Type = TVoid | TInteger | TBoolean | TClass String | TArray Type deriving Show
data Instr = Push Integer | IAdd | ISub | IMul | IDiv | ILoad Integer | IStore Integer | ALoad Integer |
             Swap | Dup | Pop | GetStatic String Type | Return | Call Signature deriving Show
