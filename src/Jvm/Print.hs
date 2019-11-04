module Jvm.Print where

import Data.Monoid
import DList
import Jvm.Grammar


showProg :: Program -> String
showProg = toList.code


b :: String -> DList Char
b s = DL (s++)


joined :: String -> [DList Char] -> DList Char
joined separator bs = foldr1 ((<>).(<> bSeparator)) bs
  where
  bSeparator = b separator


sJoined separator ss = joined separator (map b ss)


class PrintCode a where
  code :: a -> DList Char


instance PrintCode Program where
  code (Class name super modifier ms) =
    joined "\n" [bClass, bSuper, bMembers]
    where
    bClass = joined " " [(b ".class"), code modifier, (b name)]
    bSuper = sJoined " " [".super", super]
    bMembers = joined "\n\n" (map code ms)


instance PrintCode Modifier where
  code Public = b "public"


instance PrintCode Member where
  code m = b (show m)
