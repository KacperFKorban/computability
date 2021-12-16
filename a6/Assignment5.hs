{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Assignment5 where

import Assignment4
import Assignment3

import Chi

tmEval :: Exp
tmEval = parse "\
  \(\\eq.\
  \  (\\rmTr.\
  \    (\\head.\
  \      (\\findRule.\
  \        (\\act.\
  \          (\\mkResult.\
  \            (\\eval. \\tm. \\xs. case tm of\
  \              { TM(s0, trans) -> eval trans State(s0, Nil(), xs)\
  \              }\
  \            ) (\\trans. rec eval = \\state. case findRule state trans of\
  \              { Rule(s1, x1, s2, x2, d) -> eval (act state Rule(s1, x1, s2, x2, d))\
  \              ; Nothing() -> mkResult state\
  \              }\
  \            )\
  \          ) (rec mkResult = \\state. case state of\
  \            { State(s, left, tape) -> case left of\
  \              { Nil() -> rmTr tape\
  \              ; Cons(l, ls) -> mkResult State(s, ls, Cons(l, tape))\
  \              }\
  \            }\
  \          )\
  \        ) (\\state. \\rule. case state of\
  \          { State(s, left, tape) -> case rule of\
  \            { Rule(s1, x1, s2, x2, d) -> case d of\
  \              { L() -> case left of\
  \                { Nil() -> case tape of\
  \                  { Nil() -> State(s2, Nil(), Cons(x2, Nil()))\
  \                  ; Cons(x, xs) -> State(s2, Nil(), Cons(x2, xs))\
  \                  }\
  \                ; Cons(l, ls) -> case tape of\
  \                  { Nil() -> State(s2, ls, Cons(l, Cons(x2, Nil())))\
  \                  ; Cons(x, xs) -> State(s2, ls, Cons(l, Cons(x2, xs)))\
  \                  }\
  \                }\
  \              ; R() -> case tape of\
  \                { Nil() -> State(s2, Cons(x2, left), Nil())\
  \                ; Cons(r, rs) -> State(s2, Cons(x2, left), rs)\
  \                }\
  \              }\
  \            }\
  \          }\
  \        )\
  \      ) (rec findRule = \\state. \\trans. case state of\
  \        { State(s, left, tape) -> case trans of\
  \          { Nil() -> Nothing()\
  \          ; Cons(r, rs) -> case r of\
  \            { Rule(s1, x1, s2, x2, d) -> case eq s1 s of\
  \              { True() -> case eq x1 (head tape) of\
  \                { True() -> r\
  \                ; False() -> findRule state rs\
  \                }\
  \              ; False() -> findRule state rs\
  \              }\
  \            }\
  \          }\
  \        }\
  \      )\
  \    ) (\\list. case list of\
  \      { Nil() -> Blank()\
  \      ; Cons(x, xs) -> x\
  \      }\
  \    )\
  \  ) (rec rmTr = \\tape. case tape of\
  \    { Nil() -> Nil()\
  \    ; Cons(x, xs) -> case rmTr xs of\
  \      { Nil() -> case x of\
  \        { Blank() -> Nil()\
  \        ; Suc(n) -> Cons(x, Nil())\
  \        ; Zero() -> Cons(x, Nil())\
  \        }\
  \      ; Cons(x1, x1s) -> Cons(x, Cons(x1, x1s))\
  \      }\
  \    }\
  \  )\
  \) (rec eq = \\n. \\m. case n of\
  \  { Zero() -> case m of { Zero() -> True() ; Suc(m) -> False() ; Blank() -> False() }\
  \  ; Suc(n) -> case m of { Zero() -> False() ; Suc(m) -> eq n m ; Blank() -> False() }\
  \  ; Blank() -> case m of { Blank() -> True() ; Zero() -> False() ; Suc(m) -> False()}\
  \  }\
  \)\
  \"
