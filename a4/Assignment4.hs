{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Assignment4 where

import Assignment3

import Chi

substChi :: Exp
substChi = parse "\
  \(\\eq.\
  \  (\\map.\
  \    (\\elem.\
  \      (\\substBranch.\
  \        (\\x. \\e. rec subst = \\exp. case exp of\
  \          { Apply(exp1, exp2) -> Apply(subst exp1, subst exp2)\
  \          ; Lambda(var, exp1) -> case (eq var x) of\
  \            { True() -> Lambda(var, exp1)\
  \            ; False() -> Lambda(var, subst exp1)\
  \            }\
  \          ; Case(exp1, branches) -> Case(subst exp1, map (substBranch subst x e) branches)\
  \          ; Rec(var, exp1) -> case (eq var x) of\
  \            { True() -> Rec(var, exp1)\
  \            ; False() -> Rec(var, subst exp1)\
  \            }\
  \          ; Var(var) -> case (eq var x) of\
  \            { True() -> e\
  \            ; False() -> Var(var)\
  \            }\
  \          ; Const(const, exps) -> Const(const, map subst exps)\
  \          }\
  \        )\
  \      ) (\\subst. \\x. \\e. \\b. case b of\
  \        { Branch(c, vars, exp1) -> case (elem x vars) of\
  \          { True() -> Branch(c, vars, exp1)\
  \          ; False() -> Branch(c, vars, subst exp1)\
  \          }\
  \        }\
  \      )\
  \    ) (\\x. rec elem = \\vars. case vars of\
  \      { Nil() -> False()\
  \      ; Cons(v, vs) -> case (eq v x) of\
  \        { True() -> True()\
  \        ; False() -> elem vs\
  \        }\
  \      }\
  \    )\
  \  ) (\\f. rec map = \\list. case list of\
  \    { Nil() -> Nil()\
  \    ; Cons(x, xs) -> Cons(f x, map xs)\
  \    }\
  \  )\
  \) (rec eq = \\n. \\m. case n of\
  \  { Zero() -> case m of { Zero() -> True() ; Suc(m) -> False() }\
  \  ; Suc(n) -> case m of { Zero() -> False() ; Suc(m) -> eq n m }\
  \  }\
  \)\
  \"

test_substChi1 :: Bool
test_substChi1 =
  let
    v = Variable "x"
    e = Const "Z" []
    term = Rec "x" (Var "x")
    res = Rec "x" (Var "x")
  in
    runInternalSubstitution eval substChi v e term == Just res

test_substChi2 :: Bool
test_substChi2 =
  let
    v = Variable "y"
    e = Lambda "x" $ Var "x"
    term = Lambda "x" $ Apply (Var "x") (Var "y")
    res = Lambda "x" $ Apply (Var "x") (Lambda "x" $ Var "x") 
  in
    runInternalSubstitution eval substChi v e term == Just res

test_substChi3 :: Bool
test_substChi3 =
  let
    v = Variable "z"
    e = Const "C" [Lambda "z" $ Var "z"]
    term = Case (Var "z") [Branch "C" ["z"] (Var "z")]
    res = Case (Const "C" [Lambda "z" $ Var "z"]) [Branch "C" ["z"] (Var "z")]
  in
    runInternalSubstitution eval substChi v e term == Just res

test_substChi :: Bool
test_substChi = test_substChi1 && test_substChi2 && test_substChi3

evalChi :: Exp
evalChi = parse "\
  \(\\eq.\
  \  (\\map.\
  \    (\\elem.\
  \      (\\substBranch.\
  \        (\\subst.\
  \          (\\zipStrict.\
  \            (\\foldr.\
  \              (\\pick. rec eval = \\exp. case exp of\
  \                { Apply(exp1, exp2) ->\
  \                  case (eval exp1) of { Lambda(var, exp) -> eval (subst var (eval exp2) exp) }\
  \                ; Lambda(var, exp1) -> Lambda(var, exp1)\
  \                ; Case(exp1, bs) ->\
  \                  case (eval exp1) of \
  \                  { Const(c, vs) -> eval (pick vs c bs)\
  \                  }\
  \                ; Rec(var, exp) -> eval (subst var Rec(var, exp) exp)\
  \                ; Const(const, exps) -> Const(const, map eval exps)\
  \                }\
  \              ) (\\vs. \\c. rec pick = \\bs. case bs of\
  \                { Cons(b, bs) -> case b of\
  \                  { Branch(cc, xs, exp) -> case (eq cc c) of\
  \                    { True() -> foldr (\\p. \\exp. case p of\
  \                        { Pair(x, v) -> subst x v exp\
  \                        }\
  \                      ) exp (zipStrict xs vs)\
  \                    ; False() -> pick bs\
  \                    }\
  \                  }\
  \                }\
  \              )\
  \            ) (\\f. \\z. rec foldr = \\list. case list of\
  \              { Nil() -> z\
  \              ; Cons(x, xs) -> f x (foldr xs)\
  \              }\
  \            )\
  \          ) (rec zipStrict = \\l1. \\l2. case l1 of\
  \            { Nil() -> case l2 of\
  \              { Nil() -> Nil()\
  \              }\
  \            ; Cons(x, xs) -> case l2 of\
  \              { Cons(v, vs) -> Cons(Pair(x, v), zipStrict xs vs)\
  \              }\
  \            }\
  \          )\
  \        ) (\\x. \\e. rec subst = \\exp. case exp of\
  \          { Apply(exp1, exp2) -> Apply(subst exp1, subst exp2)\
  \          ; Lambda(var, exp1) -> case (eq var x) of\
  \            { True() -> Lambda(var, exp1)\
  \            ; False() -> Lambda(var, subst exp1)\
  \            }\
  \          ; Case(exp1, branches) -> Case(subst exp1, map (substBranch subst x e) branches)\
  \          ; Rec(var, exp1) -> case (eq var x) of\
  \            { True() -> Rec(var, exp1)\
  \            ; False() -> Rec(var, subst exp1)\
  \            }\
  \          ; Var(var) -> case (eq var x) of\
  \            { True() -> e\
  \            ; False() -> Var(var)\
  \            }\
  \          ; Const(const, exps) -> Const(const, map subst exps)\
  \          }\
  \        )\
  \      ) (\\subst. \\x. \\e. \\b. case b of\
  \        { Branch(c, vars, exp1) -> case (elem x vars) of\
  \          { True() -> Branch(c, vars, exp1)\
  \          ; False() -> Branch(c, vars, subst exp1)\
  \          }\
  \        }\
  \      )\
  \    ) (\\x. rec elem = \\vars. case vars of\
  \      { Nil() -> False()\
  \      ; Cons(v, vs) -> case (eq v x) of\
  \        { True() -> True()\
  \        ; False() -> elem vs\
  \        }\
  \      }\
  \    )\
  \  ) (\\f. rec map = \\list. case list of\
  \    { Nil() -> Nil()\
  \    ; Cons(x, xs) -> Cons(f x, map xs)\
  \    }\
  \  )\
  \) (rec eq = \\n. \\m. case n of\
  \  { Zero() -> case m of { Zero() -> True() ; Suc(m) -> False() }\
  \  ; Suc(n) -> case m of { Zero() -> False() ; Suc(m) -> eq n m }\
  \  }\
  \)\
  \"

runSL :: Exp -> Maybe Exp
runSL p = runDecode $ do
  term <- asDecoder (code p)
  decode (eval (Apply evalChi term))
