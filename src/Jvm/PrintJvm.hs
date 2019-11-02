module Jvm.Print where

import qualified Data.Text.Lazy.Builder as TB

import Jvm

class ShowProgram a where
  showProgram :: a -> TB.Builder

instance ShowProgram Program where
  showProgram (Class modifier members)