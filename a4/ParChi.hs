{-# OPTIONS_GHC -w #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module ParChi
  ( happyError
  , myLexer
  , pExp1
  , pExp
  , pExp2
  , pBr
  , pListBr
  , pListExp
  , pListVariable
  ) where

import Prelude

import qualified AbsChi
import LexChi
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn10 (AbsChi.Constructor)
	| HappyAbsSyn11 (AbsChi.Variable)
	| HappyAbsSyn12 (AbsChi.Exp)
	| HappyAbsSyn15 (AbsChi.Br)
	| HappyAbsSyn16 ([AbsChi.Br])
	| HappyAbsSyn17 ([AbsChi.Exp])
	| HappyAbsSyn18 ([AbsChi.Variable])

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56 :: () => Prelude.Int -> ({-HappyReduction (Err) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27 :: () => ({-HappyReduction (Err) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,129) ([0,33796,1,22544,6,64,24,0,32,0,128,4096,1624,0,4096,0,8192,0,0,0,64,0,0,0,0,0,1024,0,0,0,16384,6144,0,4,0,0,0,0,0,24640,25,0,64,1024,406,0,1024,16384,0,0,32,0,0,0,0,0,0,0,0,0,1024,384,0,0,0,2048,0,16384,0,256,0,8192,0,1024,0,512,0,1024,406,4096,1624,0,4096,0,0,0,8,0,0,0,0,0,33024,101,0,32,4096,1624,32768,0,0,0,0,32,0,0,0,0,8,0,0,0,0,0,256,16384,6496,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pExp1","%start_pExp","%start_pExp2","%start_pBr","%start_pListBr","%start_pListExp","%start_pListVariable","Constructor","Variable","Exp1","Exp","Exp2","Br","ListBr","ListExp","ListVariable","'('","')'","','","'->'","'.'","';'","'='","'\\\\'","'case'","'of'","'rec'","'{'","'}'","L_Constructor","L_Variable","%eof"]
        bit_start = st Prelude.* 34
        bit_end = (st Prelude.+ 1) Prelude.* 34
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..33]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (19) = happyShift action_18
action_0 (27) = happyShift action_20
action_0 (32) = happyShift action_8
action_0 (33) = happyShift action_11
action_0 (10) = happyGoto action_12
action_0 (11) = happyGoto action_13
action_0 (12) = happyGoto action_28
action_0 (14) = happyGoto action_16
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (19) = happyShift action_18
action_1 (26) = happyShift action_19
action_1 (27) = happyShift action_20
action_1 (29) = happyShift action_21
action_1 (32) = happyShift action_8
action_1 (33) = happyShift action_11
action_1 (10) = happyGoto action_12
action_1 (11) = happyGoto action_13
action_1 (12) = happyGoto action_14
action_1 (13) = happyGoto action_27
action_1 (14) = happyGoto action_16
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (19) = happyShift action_18
action_2 (32) = happyShift action_8
action_2 (33) = happyShift action_11
action_2 (10) = happyGoto action_12
action_2 (11) = happyGoto action_13
action_2 (14) = happyGoto action_26
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (32) = happyShift action_8
action_3 (10) = happyGoto action_22
action_3 (15) = happyGoto action_25
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (32) = happyShift action_8
action_4 (10) = happyGoto action_22
action_4 (15) = happyGoto action_23
action_4 (16) = happyGoto action_24
action_4 _ = happyReduce_19

action_5 (19) = happyShift action_18
action_5 (26) = happyShift action_19
action_5 (27) = happyShift action_20
action_5 (29) = happyShift action_21
action_5 (32) = happyShift action_8
action_5 (33) = happyShift action_11
action_5 (10) = happyGoto action_12
action_5 (11) = happyGoto action_13
action_5 (12) = happyGoto action_14
action_5 (13) = happyGoto action_15
action_5 (14) = happyGoto action_16
action_5 (17) = happyGoto action_17
action_5 _ = happyReduce_22

action_6 (33) = happyShift action_11
action_6 (11) = happyGoto action_9
action_6 (18) = happyGoto action_10
action_6 _ = happyReduce_25

action_7 (32) = happyShift action_8
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_7

action_9 (21) = happyShift action_38
action_9 _ = happyReduce_26

action_10 (34) = happyAccept
action_10 _ = happyFail (happyExpListPerState 10)

action_11 _ = happyReduce_8

action_12 (19) = happyShift action_37
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_15

action_14 (19) = happyShift action_18
action_14 (32) = happyShift action_8
action_14 (33) = happyShift action_11
action_14 (10) = happyGoto action_12
action_14 (11) = happyGoto action_13
action_14 (14) = happyGoto action_29
action_14 _ = happyReduce_14

action_15 (21) = happyShift action_36
action_15 _ = happyReduce_23

action_16 _ = happyReduce_11

action_17 (34) = happyAccept
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (19) = happyShift action_18
action_18 (26) = happyShift action_19
action_18 (27) = happyShift action_20
action_18 (29) = happyShift action_21
action_18 (32) = happyShift action_8
action_18 (33) = happyShift action_11
action_18 (10) = happyGoto action_12
action_18 (11) = happyGoto action_13
action_18 (12) = happyGoto action_14
action_18 (13) = happyGoto action_35
action_18 (14) = happyGoto action_16
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (33) = happyShift action_11
action_19 (11) = happyGoto action_34
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (19) = happyShift action_18
action_20 (26) = happyShift action_19
action_20 (27) = happyShift action_20
action_20 (29) = happyShift action_21
action_20 (32) = happyShift action_8
action_20 (33) = happyShift action_11
action_20 (10) = happyGoto action_12
action_20 (11) = happyGoto action_13
action_20 (12) = happyGoto action_14
action_20 (13) = happyGoto action_33
action_20 (14) = happyGoto action_16
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (33) = happyShift action_11
action_21 (11) = happyGoto action_32
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (19) = happyShift action_31
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (24) = happyShift action_30
action_23 _ = happyReduce_20

action_24 (34) = happyAccept
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (34) = happyAccept
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (34) = happyAccept
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (34) = happyAccept
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (19) = happyShift action_18
action_28 (32) = happyShift action_8
action_28 (33) = happyShift action_11
action_28 (34) = happyAccept
action_28 (10) = happyGoto action_12
action_28 (11) = happyGoto action_13
action_28 (14) = happyGoto action_29
action_28 _ = happyFail (happyExpListPerState 28)

action_29 _ = happyReduce_9

action_30 (32) = happyShift action_8
action_30 (10) = happyGoto action_22
action_30 (15) = happyGoto action_23
action_30 (16) = happyGoto action_47
action_30 _ = happyReduce_19

action_31 (33) = happyShift action_11
action_31 (11) = happyGoto action_9
action_31 (18) = happyGoto action_46
action_31 _ = happyReduce_25

action_32 (25) = happyShift action_45
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (28) = happyShift action_44
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (23) = happyShift action_43
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (20) = happyShift action_42
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (19) = happyShift action_18
action_36 (26) = happyShift action_19
action_36 (27) = happyShift action_20
action_36 (29) = happyShift action_21
action_36 (32) = happyShift action_8
action_36 (33) = happyShift action_11
action_36 (10) = happyGoto action_12
action_36 (11) = happyGoto action_13
action_36 (12) = happyGoto action_14
action_36 (13) = happyGoto action_15
action_36 (14) = happyGoto action_16
action_36 (17) = happyGoto action_41
action_36 _ = happyReduce_22

action_37 (19) = happyShift action_18
action_37 (26) = happyShift action_19
action_37 (27) = happyShift action_20
action_37 (29) = happyShift action_21
action_37 (32) = happyShift action_8
action_37 (33) = happyShift action_11
action_37 (10) = happyGoto action_12
action_37 (11) = happyGoto action_13
action_37 (12) = happyGoto action_14
action_37 (13) = happyGoto action_15
action_37 (14) = happyGoto action_16
action_37 (17) = happyGoto action_40
action_37 _ = happyReduce_22

action_38 (33) = happyShift action_11
action_38 (11) = happyGoto action_9
action_38 (18) = happyGoto action_39
action_38 _ = happyReduce_25

action_39 _ = happyReduce_27

action_40 (20) = happyShift action_52
action_40 _ = happyFail (happyExpListPerState 40)

action_41 _ = happyReduce_24

action_42 _ = happyReduce_17

action_43 (19) = happyShift action_18
action_43 (26) = happyShift action_19
action_43 (27) = happyShift action_20
action_43 (29) = happyShift action_21
action_43 (32) = happyShift action_8
action_43 (33) = happyShift action_11
action_43 (10) = happyGoto action_12
action_43 (11) = happyGoto action_13
action_43 (12) = happyGoto action_14
action_43 (13) = happyGoto action_51
action_43 (14) = happyGoto action_16
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (30) = happyShift action_50
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (19) = happyShift action_18
action_45 (26) = happyShift action_19
action_45 (27) = happyShift action_20
action_45 (29) = happyShift action_21
action_45 (32) = happyShift action_8
action_45 (33) = happyShift action_11
action_45 (10) = happyGoto action_12
action_45 (11) = happyGoto action_13
action_45 (12) = happyGoto action_14
action_45 (13) = happyGoto action_49
action_45 (14) = happyGoto action_16
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (20) = happyShift action_48
action_46 _ = happyFail (happyExpListPerState 46)

action_47 _ = happyReduce_21

action_48 (22) = happyShift action_54
action_48 _ = happyFail (happyExpListPerState 48)

action_49 _ = happyReduce_13

action_50 (32) = happyShift action_8
action_50 (10) = happyGoto action_22
action_50 (15) = happyGoto action_23
action_50 (16) = happyGoto action_53
action_50 _ = happyReduce_19

action_51 _ = happyReduce_12

action_52 _ = happyReduce_16

action_53 (31) = happyShift action_56
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (19) = happyShift action_18
action_54 (26) = happyShift action_19
action_54 (27) = happyShift action_20
action_54 (29) = happyShift action_21
action_54 (32) = happyShift action_8
action_54 (33) = happyShift action_11
action_54 (10) = happyGoto action_12
action_54 (11) = happyGoto action_13
action_54 (12) = happyGoto action_14
action_54 (13) = happyGoto action_55
action_54 (14) = happyGoto action_16
action_54 _ = happyFail (happyExpListPerState 54)

action_55 _ = happyReduce_18

action_56 _ = happyReduce_10

happyReduce_7 = happySpecReduce_1  10 happyReduction_7
happyReduction_7 (HappyTerminal (PT _ (T_Constructor happy_var_1)))
	 =  HappyAbsSyn10
		 (AbsChi.Constructor happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  11 happyReduction_8
happyReduction_8 (HappyTerminal (PT _ (T_Variable happy_var_1)))
	 =  HappyAbsSyn11
		 (AbsChi.Variable happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  12 happyReduction_9
happyReduction_9 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AbsChi.Apply happy_var_1 happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happyReduce 6 12 happyReduction_10
happyReduction_10 (_ `HappyStk`
	(HappyAbsSyn16  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (AbsChi.Case happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_1  12 happyReduction_11
happyReduction_11 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happyReduce 4 13 happyReduction_12
happyReduction_12 ((HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (AbsChi.Lambda happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 4 13 happyReduction_13
happyReduction_13 ((HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (AbsChi.Rec happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_14 = happySpecReduce_1  13 happyReduction_14
happyReduction_14 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  14 happyReduction_15
happyReduction_15 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn12
		 (AbsChi.Var happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happyReduce 4 14 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (AbsChi.Const happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_3  14 happyReduction_17
happyReduction_17 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 6 15 happyReduction_18
happyReduction_18 ((HappyAbsSyn12  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (AbsChi.Branch happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_0  16 happyReduction_19
happyReduction_19  =  HappyAbsSyn16
		 ([]
	)

happyReduce_20 = happySpecReduce_1  16 happyReduction_20
happyReduction_20 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn16
		 ((:[]) happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  16 happyReduction_21
happyReduction_21 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn16
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_0  17 happyReduction_22
happyReduction_22  =  HappyAbsSyn17
		 ([]
	)

happyReduce_23 = happySpecReduce_1  17 happyReduction_23
happyReduction_23 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn17
		 ((:[]) happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  17 happyReduction_24
happyReduction_24 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn17
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_0  18 happyReduction_25
happyReduction_25  =  HappyAbsSyn18
		 ([]
	)

happyReduce_26 = happySpecReduce_1  18 happyReduction_26
happyReduction_26 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn18
		 ((:[]) happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  18 happyReduction_27
happyReduction_27 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn18
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 34 34 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 19;
	PT _ (TS _ 2) -> cont 20;
	PT _ (TS _ 3) -> cont 21;
	PT _ (TS _ 4) -> cont 22;
	PT _ (TS _ 5) -> cont 23;
	PT _ (TS _ 6) -> cont 24;
	PT _ (TS _ 7) -> cont 25;
	PT _ (TS _ 8) -> cont 26;
	PT _ (TS _ 9) -> cont 27;
	PT _ (TS _ 10) -> cont 28;
	PT _ (TS _ 11) -> cont 29;
	PT _ (TS _ 12) -> cont 30;
	PT _ (TS _ 13) -> cont 31;
	PT _ (T_Constructor happy_dollar_dollar) -> cont 32;
	PT _ (T_Variable happy_dollar_dollar) -> cont 33;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 34 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = ((>>=))
happyReturn :: () => a -> Err a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pExp1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn12 z -> happyReturn z; _other -> notHappyAtAll })

pExp tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn12 z -> happyReturn z; _other -> notHappyAtAll })

pExp2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn12 z -> happyReturn z; _other -> notHappyAtAll })

pBr tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_3 tks) (\x -> case x of {HappyAbsSyn15 z -> happyReturn z; _other -> notHappyAtAll })

pListBr tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_4 tks) (\x -> case x of {HappyAbsSyn16 z -> happyReturn z; _other -> notHappyAtAll })

pListExp tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_5 tks) (\x -> case x of {HappyAbsSyn17 z -> happyReturn z; _other -> notHappyAtAll })

pListVariable tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_6 tks) (\x -> case x of {HappyAbsSyn18 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
