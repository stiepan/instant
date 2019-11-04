module Jvm.AbsInstant where

import qualified AbsGrammar as A (Exp, Ident, )

data Program = Prog [Stmt]
  deriving (Eq, Ord, Show, Read)

data Stmt = SAss A.Ident A.Exp | SExp A.Exp
  deriving (Eq, Ord, Show, Read)

data Exp = Exp Integer A.Exp  deriving (Eq, Ord, Show, Read)

