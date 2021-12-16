-- File generated by the BNF Converter (bnfc 2.9.3).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module SkelChi where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified AbsChi

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transConstructor :: AbsChi.Constructor -> Result
transConstructor x = case x of
  AbsChi.Constructor string -> failure x

transVariable :: AbsChi.Variable -> Result
transVariable x = case x of
  AbsChi.Variable string -> failure x

transExp :: AbsChi.Exp -> Result
transExp x = case x of
  AbsChi.Apply exp1 exp2 -> failure x
  AbsChi.Lambda variable exp -> failure x
  AbsChi.Case exp brs -> failure x
  AbsChi.Rec variable exp -> failure x
  AbsChi.Var variable -> failure x
  AbsChi.Const constructor exps -> failure x

transBr :: AbsChi.Br -> Result
transBr x = case x of
  AbsChi.Branch constructor variables exp -> failure x
