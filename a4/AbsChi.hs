-- File generated by the BNF Converter (bnfc 2.9.3).

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The abstract syntax of language Chi.

module AbsChi where

import Prelude (String)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String

data Exp
    = Apply Exp Exp
    | Lambda Variable Exp
    | Case Exp [Br]
    | Rec Variable Exp
    | Var Variable
    | Const Constructor [Exp]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Br = Branch Constructor [Variable] Exp
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Constructor = Constructor String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

newtype Variable = Variable String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)