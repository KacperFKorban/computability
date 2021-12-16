{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Assignment3 where

import Chi

subst :: Variable -> Exp -> Exp -> Exp
subst v e =
  let go = subst v e
  in \case
  Apply exp1 exp2 ->
    Apply (go exp1) (go exp2)
  original@(Lambda var exp1) ->
    if v == var
      then original
      else Lambda var (go exp1)
  Case exp1 branches ->
    Case (go exp1) (substBranches v e <$> branches)
  original@(Rec var exp) ->
    if v == var
      then original
      else Rec var (go exp)
  original@(Var var) ->
    if v == var
      then e
      else original
  Const const exps ->
    Const const (go <$> exps)

substBranches :: Variable -> Exp -> Br -> Br
substBranches v e = \case
  original@(Branch c vars exp) ->
    if v `elem` vars
      then original
      else Branch c vars (subst v e exp)

test_subst1 :: Bool
test_subst1 =
  let
    v = Variable "x"
    e = Const "Z" []
    term = Rec "x" (Var "x")
    res = Rec "x" (Var "x")
  in
    subst v e term == res

test_subst2 :: Bool
test_subst2 =
  let
    v = Variable "y"
    e = Lambda "x" $ Var "x"
    term = Lambda "x" $ Apply (Var "x") (Var "y")
    res = Lambda "x" $ Apply (Var "x") (Lambda "x" $ Var "x") 
  in
    subst v e term == res

test_subst3 :: Bool
test_subst3 =
  let
    v = Variable "z"
    e = Const "C" [Lambda "z" $ Var "z"]
    term = Case (Var "z") [Branch "C" ["z"] (Var "z")]
    res = Case (Const "C" [Lambda "z" $ Var "z"]) [Branch "C" ["z"] (Var "z")]
  in
    subst v e term == res

test_subst :: Bool
test_subst = test_subst1 && test_subst2 && test_subst3

mul = subst "add" add $ 
  Rec
    (Variable "mul")
    (Lambda
      (Variable "x")
      (Lambda
        (Variable "y")
        (Case
          (Var (Variable "x"))
          [
            Branch
              (Constructor "Zero")
              []
              (Const (Constructor "Zero") []),
            Branch
              (Constructor "Suc")
              [Variable "n"]
              (Apply
                (Apply (Var (Variable "add")) (Var (Variable "y")))
                (Apply (Apply (Var (Variable "mul")) (Var (Variable "n"))) (Var (Variable "y")))
              )
          ]
        )
      )
    )

test_mul :: Bool
test_mul =
  let
    nums = (,) <$> [1..10] <*> [1..10]
  in
    all (\(n, m) ->
      let
        nn = fromNatural n
        mn = fromNatural m
      in
        toNatural (eval (Apply (Apply mul nn) mn)) == Just (n * m)
    ) nums

eval :: Exp -> Exp
eval = \case
  Apply exp1 exp2 ->
    case eval exp1 of
      Lambda var exp ->
        eval $ subst var (eval exp2) exp
  original@(Lambda var exp1) ->
    original
  Case exp1 bs ->
    case eval exp1 of
      Const c vs ->
        let
          (xs, e') = lookp c bs
          e'' = foldr (\(x, v) exp -> subst x v exp) e' (zipStrict xs vs)
        in
          eval e''
  original@(Rec var exp) ->
    eval $ subst var original exp
  Const const exps ->
    Const const $ eval <$> exps
  exp -> error $ "Unhandled expression " ++ show exp

lookp :: Constructor -> [Br] -> ([Variable], Exp)
lookp c (Branch c' xs exp : _) | c == c' = (xs, exp)
lookp c (b : bs) = lookp c bs

zipStrict :: [Variable] -> [Exp] -> [(Variable, Exp)]
zipStrict [] [] = []
zipStrict (x : xs) (v : vs) = (x, v) : zipStrict xs vs

wrong1 :: Exp
wrong1 = Apply (Const "C" []) (Const "C" [])

wrong2 :: Exp
wrong2 = Case (Lambda "x" (Var "x")) []

wrong3 :: Exp
wrong3 = Case (Const "C" []) [Branch "C" ["x"] (Var "x")]

wrong4 :: Exp
wrong4 = Case (Const "C" [Const "C" []]) [Branch "C" [] (Const "C" [])]

wrong5 :: Exp
wrong5 = Case (Const "C" [Const "C" []]) [Branch "C" [] (Const "C" []), Branch "C" ["x"] (Var "x")]

wrong6 :: Exp
wrong6 = Case (Const "C" []) [Branch "D" [] (Const "D" [])]

-- should be -- Const "E" []
right1 :: Exp
right1 = Case (Const "C" [Const "D" [], Const "E" []]) [Branch "C" ["x", "x"] (Var "x")]

-- should be -- Const "Zero" []
right2 :: Exp
right2 = Case (Const "C" [Lambda "x" (Var "x"), Const "Zero" []]) [Branch "C" ["f", "x"] (Apply (Var "f") (Var "x"))]

-- should be -- Const "C" []
right3 :: Exp
right3 = Case (Apply (Lambda "x" (Var "x")) (Const "C" [])) [Branch "C" [] (Const "C" [])]

-- should be -- Lambda "x" (Var "x")
right4 :: Exp
right4 = Apply  (Apply (Lambda "x" (Var "x")) (Lambda "x" (Var "x"))) (Lambda "x" (Var "x"))
