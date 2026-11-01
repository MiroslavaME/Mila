{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
module Grammars where

import Lexer (Token(..))
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10

happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x1c\x80\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\xff\x0d\x00\x07\x20\x00\x1c\x80\x00\x70\x00\x02\xc0\x01\x08\x00\x00\x20\x00\x00\x80\x00\x00\x00\x02\xc0\x01\x08\x00\x00\x20\x00\x00\x80\x00\x10\x00\x00\xc0\x01\x08\x00\x07\x20\x00\x04\x00\x00\x00\x00\x02\x40\x00\x00\x00\x00\x40\x00\x1c\x80\x00\x00\x00\x04\x00\x00\x10\x00\x07\x20\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x70\x00\x02\x00\x00\x00\x00\x00\x00\x00\x1c\x80\x00\x00\x00\x04\x40\x00\x00\x00\x00\x40\x00\x04\x00\x00\x70\x00\x02\xc0\x01\x08\x00\x07\x20\x00\x00\x00\x01\x00\x00\x04\x00\x00\x10\x00\x00\x00\x00\x1c\x80\x00\x70\x00\x02\xc0\x01\x08\x00\x00\x40\x00\x1c\x80\x00\x00\x00\x04\x00\x00\x10\x00\x00\x40\x00\x00\x00\x00\x00\x00\x02\xc0\x01\x08\x00\x00\x40\x00\x00\x00\x01\x00\x00\x00\xc0\x01\x0a\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x70\x00\x02\xc0\x01\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x10\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","SASA","Clauses","ElseClause","Params","Arguments","Operands","Bindings","var","nat","bool","'+'","'-'","\"not\"","\"let\"","\"let*\"","\"lambda\"","\"if\"","\"cond\"","\"else\"","\"letrec\"","'('","')'","%eof"]
        bit_start = st Prelude.* 26
        bit_end = (st Prelude.+ 1) Prelude.* 26
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..25]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (11#) = happyShift action_2
action_0 (12#) = happyShift action_4
action_0 (13#) = happyShift action_5
action_0 (24#) = happyShift action_6
action_0 (4#) = happyGoto action_3
action_0 x = happyTcHack x happyFail (happyExpListPerState 0)

action_1 (11#) = happyShift action_2
action_1 x = happyTcHack x happyFail (happyExpListPerState 1)

action_2 x = happyTcHack x happyReduce_1

action_3 (26#) = happyAccept
action_3 x = happyTcHack x happyFail (happyExpListPerState 3)

action_4 x = happyTcHack x happyReduce_2

action_5 x = happyTcHack x happyReduce_3

action_6 (11#) = happyShift action_2
action_6 (12#) = happyShift action_4
action_6 (13#) = happyShift action_5
action_6 (14#) = happyShift action_8
action_6 (15#) = happyShift action_9
action_6 (16#) = happyShift action_10
action_6 (17#) = happyShift action_11
action_6 (18#) = happyShift action_12
action_6 (19#) = happyShift action_13
action_6 (20#) = happyShift action_14
action_6 (21#) = happyShift action_15
action_6 (23#) = happyShift action_16
action_6 (24#) = happyShift action_6
action_6 (4#) = happyGoto action_7
action_6 x = happyTcHack x happyFail (happyExpListPerState 6)

action_7 (11#) = happyShift action_2
action_7 (12#) = happyShift action_4
action_7 (13#) = happyShift action_5
action_7 (24#) = happyShift action_6
action_7 (4#) = happyGoto action_27
action_7 (8#) = happyGoto action_28
action_7 x = happyTcHack x happyFail (happyExpListPerState 7)

action_8 (11#) = happyShift action_2
action_8 (12#) = happyShift action_4
action_8 (13#) = happyShift action_5
action_8 (24#) = happyShift action_6
action_8 (4#) = happyGoto action_24
action_8 (9#) = happyGoto action_26
action_8 x = happyTcHack x happyFail (happyExpListPerState 8)

action_9 (11#) = happyShift action_2
action_9 (12#) = happyShift action_4
action_9 (13#) = happyShift action_5
action_9 (24#) = happyShift action_6
action_9 (4#) = happyGoto action_24
action_9 (9#) = happyGoto action_25
action_9 x = happyTcHack x happyFail (happyExpListPerState 9)

action_10 (11#) = happyShift action_2
action_10 (12#) = happyShift action_4
action_10 (13#) = happyShift action_5
action_10 (24#) = happyShift action_6
action_10 (4#) = happyGoto action_23
action_10 x = happyTcHack x happyFail (happyExpListPerState 10)

action_11 (24#) = happyShift action_22
action_11 x = happyTcHack x happyFail (happyExpListPerState 11)

action_12 (24#) = happyShift action_21
action_12 x = happyTcHack x happyFail (happyExpListPerState 12)

action_13 (24#) = happyShift action_20
action_13 x = happyTcHack x happyFail (happyExpListPerState 13)

action_14 (11#) = happyShift action_2
action_14 (12#) = happyShift action_4
action_14 (13#) = happyShift action_5
action_14 (24#) = happyShift action_6
action_14 (4#) = happyGoto action_19
action_14 x = happyTcHack x happyFail (happyExpListPerState 14)

action_15 (24#) = happyShift action_18
action_15 x = happyTcHack x happyFail (happyExpListPerState 15)

action_16 (24#) = happyShift action_17
action_16 x = happyTcHack x happyFail (happyExpListPerState 16)

action_17 (11#) = happyShift action_43
action_17 x = happyTcHack x happyFail (happyExpListPerState 17)

action_18 (11#) = happyShift action_2
action_18 (12#) = happyShift action_4
action_18 (13#) = happyShift action_5
action_18 (24#) = happyShift action_6
action_18 (4#) = happyGoto action_42
action_18 x = happyTcHack x happyFail (happyExpListPerState 18)

action_19 (11#) = happyShift action_2
action_19 (12#) = happyShift action_4
action_19 (13#) = happyShift action_5
action_19 (24#) = happyShift action_6
action_19 (4#) = happyGoto action_41
action_19 x = happyTcHack x happyFail (happyExpListPerState 19)

action_20 (11#) = happyShift action_40
action_20 (7#) = happyGoto action_39
action_20 x = happyTcHack x happyFail (happyExpListPerState 20)

action_21 (24#) = happyShift action_38
action_21 (10#) = happyGoto action_37
action_21 x = happyTcHack x happyFail (happyExpListPerState 21)

action_22 (11#) = happyShift action_36
action_22 x = happyTcHack x happyFail (happyExpListPerState 22)

action_23 (25#) = happyShift action_35
action_23 x = happyTcHack x happyFail (happyExpListPerState 23)

action_24 (11#) = happyShift action_2
action_24 (12#) = happyShift action_4
action_24 (13#) = happyShift action_5
action_24 (24#) = happyShift action_6
action_24 (4#) = happyGoto action_33
action_24 (9#) = happyGoto action_34
action_24 x = happyTcHack x happyFail (happyExpListPerState 24)

action_25 (25#) = happyShift action_32
action_25 x = happyTcHack x happyFail (happyExpListPerState 25)

action_26 (25#) = happyShift action_31
action_26 x = happyTcHack x happyFail (happyExpListPerState 26)

action_27 (11#) = happyShift action_2
action_27 (12#) = happyShift action_4
action_27 (13#) = happyShift action_5
action_27 (24#) = happyShift action_6
action_27 (4#) = happyGoto action_27
action_27 (8#) = happyGoto action_30
action_27 x = happyTcHack x happyReduce_19

action_28 (25#) = happyShift action_29
action_28 x = happyTcHack x happyFail (happyExpListPerState 28)

action_29 x = happyTcHack x happyReduce_10

action_30 x = happyTcHack x happyReduce_20

action_31 x = happyTcHack x happyReduce_4

action_32 x = happyTcHack x happyReduce_5

action_33 (11#) = happyShift action_2
action_33 (12#) = happyShift action_4
action_33 (13#) = happyShift action_5
action_33 (24#) = happyShift action_6
action_33 (4#) = happyGoto action_33
action_33 (9#) = happyGoto action_34
action_33 x = happyTcHack x happyReduce_21

action_34 x = happyTcHack x happyReduce_22

action_35 x = happyTcHack x happyReduce_6

action_36 (11#) = happyShift action_2
action_36 (12#) = happyShift action_4
action_36 (13#) = happyShift action_5
action_36 (24#) = happyShift action_6
action_36 (4#) = happyGoto action_51
action_36 x = happyTcHack x happyFail (happyExpListPerState 36)

action_37 (25#) = happyShift action_50
action_37 x = happyTcHack x happyFail (happyExpListPerState 37)

action_38 (11#) = happyShift action_49
action_38 x = happyTcHack x happyFail (happyExpListPerState 38)

action_39 (25#) = happyShift action_48
action_39 x = happyTcHack x happyFail (happyExpListPerState 39)

action_40 (11#) = happyShift action_40
action_40 (7#) = happyGoto action_47
action_40 x = happyTcHack x happyReduce_17

action_41 (11#) = happyShift action_2
action_41 (12#) = happyShift action_4
action_41 (13#) = happyShift action_5
action_41 (24#) = happyShift action_6
action_41 (4#) = happyGoto action_46
action_41 x = happyTcHack x happyFail (happyExpListPerState 41)

action_42 (11#) = happyShift action_2
action_42 (12#) = happyShift action_4
action_42 (13#) = happyShift action_5
action_42 (24#) = happyShift action_6
action_42 (4#) = happyGoto action_45
action_42 x = happyTcHack x happyFail (happyExpListPerState 42)

action_43 (11#) = happyShift action_2
action_43 (12#) = happyShift action_4
action_43 (13#) = happyShift action_5
action_43 (24#) = happyShift action_6
action_43 (4#) = happyGoto action_44
action_43 x = happyTcHack x happyFail (happyExpListPerState 43)

action_44 (25#) = happyShift action_58
action_44 x = happyTcHack x happyFail (happyExpListPerState 44)

action_45 (25#) = happyShift action_57
action_45 x = happyTcHack x happyFail (happyExpListPerState 45)

action_46 (25#) = happyShift action_56
action_46 x = happyTcHack x happyFail (happyExpListPerState 46)

action_47 x = happyTcHack x happyReduce_18

action_48 (11#) = happyShift action_2
action_48 (12#) = happyShift action_4
action_48 (13#) = happyShift action_5
action_48 (24#) = happyShift action_6
action_48 (4#) = happyGoto action_55
action_48 x = happyTcHack x happyFail (happyExpListPerState 48)

action_49 (11#) = happyShift action_2
action_49 (12#) = happyShift action_4
action_49 (13#) = happyShift action_5
action_49 (24#) = happyShift action_6
action_49 (4#) = happyGoto action_54
action_49 x = happyTcHack x happyFail (happyExpListPerState 49)

action_50 (11#) = happyShift action_2
action_50 (12#) = happyShift action_4
action_50 (13#) = happyShift action_5
action_50 (24#) = happyShift action_6
action_50 (4#) = happyGoto action_53
action_50 x = happyTcHack x happyFail (happyExpListPerState 50)

action_51 (25#) = happyShift action_52
action_51 x = happyTcHack x happyFail (happyExpListPerState 51)

action_52 (11#) = happyShift action_2
action_52 (12#) = happyShift action_4
action_52 (13#) = happyShift action_5
action_52 (24#) = happyShift action_6
action_52 (4#) = happyGoto action_66
action_52 x = happyTcHack x happyFail (happyExpListPerState 52)

action_53 (25#) = happyShift action_65
action_53 x = happyTcHack x happyFail (happyExpListPerState 53)

action_54 (25#) = happyShift action_64
action_54 x = happyTcHack x happyFail (happyExpListPerState 54)

action_55 (25#) = happyShift action_63
action_55 x = happyTcHack x happyFail (happyExpListPerState 55)

action_56 x = happyTcHack x happyReduce_11

action_57 (24#) = happyShift action_62
action_57 (5#) = happyGoto action_60
action_57 (6#) = happyGoto action_61
action_57 x = happyTcHack x happyFail (happyExpListPerState 57)

action_58 (11#) = happyShift action_2
action_58 (12#) = happyShift action_4
action_58 (13#) = happyShift action_5
action_58 (24#) = happyShift action_6
action_58 (4#) = happyGoto action_59
action_58 x = happyTcHack x happyFail (happyExpListPerState 58)

action_59 (25#) = happyShift action_72
action_59 x = happyTcHack x happyFail (happyExpListPerState 59)

action_60 (25#) = happyShift action_71
action_60 x = happyTcHack x happyFail (happyExpListPerState 60)

action_61 x = happyTcHack x happyReduce_14

action_62 (11#) = happyShift action_2
action_62 (12#) = happyShift action_4
action_62 (13#) = happyShift action_5
action_62 (22#) = happyShift action_70
action_62 (24#) = happyShift action_6
action_62 (4#) = happyGoto action_69
action_62 x = happyTcHack x happyFail (happyExpListPerState 62)

action_63 x = happyTcHack x happyReduce_9

action_64 (24#) = happyShift action_38
action_64 (10#) = happyGoto action_68
action_64 x = happyTcHack x happyReduce_23

action_65 x = happyTcHack x happyReduce_8

action_66 (25#) = happyShift action_67
action_66 x = happyTcHack x happyFail (happyExpListPerState 66)

action_67 x = happyTcHack x happyReduce_7

action_68 x = happyTcHack x happyReduce_24

action_69 (11#) = happyShift action_2
action_69 (12#) = happyShift action_4
action_69 (13#) = happyShift action_5
action_69 (24#) = happyShift action_6
action_69 (4#) = happyGoto action_74
action_69 x = happyTcHack x happyFail (happyExpListPerState 69)

action_70 (11#) = happyShift action_2
action_70 (12#) = happyShift action_4
action_70 (13#) = happyShift action_5
action_70 (24#) = happyShift action_6
action_70 (4#) = happyGoto action_73
action_70 x = happyTcHack x happyFail (happyExpListPerState 70)

action_71 x = happyTcHack x happyReduce_12

action_72 x = happyTcHack x happyReduce_13

action_73 (25#) = happyShift action_76
action_73 x = happyTcHack x happyFail (happyExpListPerState 73)

action_74 (25#) = happyShift action_75
action_74 x = happyTcHack x happyFail (happyExpListPerState 74)

action_75 (24#) = happyShift action_62
action_75 (5#) = happyGoto action_77
action_75 (6#) = happyGoto action_61
action_75 x = happyTcHack x happyFail (happyExpListPerState 75)

action_76 x = happyTcHack x happyReduce_16

action_77 x = happyTcHack x happyReduce_15

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_1 = happySpecReduce_1  4# happyReduction_1
happyReduction_1 (HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn4
		 (IdS happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_2 = happySpecReduce_1  4# happyReduction_2
happyReduction_2 (HappyTerminal (TokenNum happy_var_1))
	 =  HappyAbsSyn4
		 (NumS happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_3 = happySpecReduce_1  4# happyReduction_3
happyReduction_3 (HappyTerminal (TokenBool happy_var_1))
	 =  HappyAbsSyn4
		 (BooleanS happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_4 = happyReduce 4# 4# happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (AddS happy_var_3
	) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_5 = happyReduce 4# 4# happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (SubS happy_var_3
	) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_6 = happyReduce 4# 4# happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (NotS happy_var_3
	) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_7 = happyReduce 8# 4# happyReduction_7
happyReduction_7 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_5) `HappyStk`
	(HappyTerminal (TokenId happy_var_4)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (LetS happy_var_4 happy_var_5 happy_var_7
	) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_8 = happyReduce 7# 4# happyReduction_8
happyReduction_8 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (LetStarS happy_var_4 happy_var_6
	) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_9 = happyReduce 7# 4# happyReduction_9
happyReduction_9 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (FunS happy_var_4 happy_var_6
	) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_10 = happyReduce 4# 4# happyReduction_10
happyReduction_10 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (AppS happy_var_2 happy_var_3
	) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_11 = happyReduce 6# 4# happyReduction_11
happyReduction_11 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_5) `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (IfS happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_12 = happyReduce 8# 4# happyReduction_12
happyReduction_12 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_5) `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (CondS ((happy_var_4, happy_var_5) : fst happy_var_7) (snd happy_var_7)
	) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_13 = happyReduce 8# 4# happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_5) `HappyStk`
	(HappyTerminal (TokenId happy_var_4)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (LetRecS happy_var_4 happy_var_5 happy_var_7
	) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_14 = happySpecReduce_1  5# happyReduction_14
happyReduction_14 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (([], happy_var_1)
	)
happyReduction_14 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_15 = happyReduce 5# 5# happyReduction_15
happyReduction_15 ((HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (((happy_var_2, happy_var_3) : fst happy_var_5, snd happy_var_5)
	) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_16 = happyReduce 4# 6# happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (happy_var_3
	) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_17 = happySpecReduce_1  7# happyReduction_17
happyReduction_17 (HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_17 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_18 = happySpecReduce_2  7# happyReduction_18
happyReduction_18 (HappyAbsSyn7  happy_var_2)
	(HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_2
	)
happyReduction_18 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_19 = happySpecReduce_1  8# happyReduction_19
happyReduction_19 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_19 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_20 = happySpecReduce_2  8# happyReduction_20
happyReduction_20 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 : happy_var_2
	)
happyReduction_20 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_21 = happySpecReduce_2  9# happyReduction_21
happyReduction_21 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1, happy_var_2]
	)
happyReduction_21 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_22 = happySpecReduce_2  9# happyReduction_22
happyReduction_22 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 : happy_var_2
	)
happyReduction_22 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_23 = happyReduce 4# 10# happyReduction_23
happyReduction_23 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	(HappyTerminal (TokenId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 ([(happy_var_2, happy_var_3)]
	) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_24 = happyReduce 5# 10# happyReduction_24
happyReduction_24 ((HappyAbsSyn10  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	(HappyTerminal (TokenId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 ((happy_var_2, happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 26# 26# notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenId happy_dollar_dollar -> cont 11#;
	TokenNum happy_dollar_dollar -> cont 12#;
	TokenBool happy_dollar_dollar -> cont 13#;
	TokenSuma -> cont 14#;
	TokenResta -> cont 15#;
	TokenNot -> cont 16#;
	TokenLet -> cont 17#;
	TokenLetStar -> cont 18#;
	TokenLambda -> cont 19#;
	TokenIf -> cont 20#;
	TokenCond -> cont 21#;
	TokenElse -> cont 22#;
	TokenLetRec -> cont 23#;
	TokenPA -> cont 24#;
	TokenPC -> cont 25#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 26# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
parse tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError tokens = error ("Parse error: " ++ show tokens)

type Nombre = String

data SASA
  = IdS Nombre
  | NumS Int
  | BooleanS Bool
  | AddS [SASA]
  | SubS [SASA]
  | NotS SASA
  | LetS Nombre SASA SASA
  | LetStarS [(Nombre, SASA)] SASA
  | FunS [Nombre] SASA
  | AppS SASA [SASA]
  | IfS SASA SASA SASA
  | CondS [(SASA, SASA)] SASA
  | LetRecS Nombre SASA SASA
  deriving (Eq, Show)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Prelude.Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Prelude.Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Prelude.Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif



















data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList








































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
happyAccept 1# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j ) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

































indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `Prelude.mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Happy_GHC_Exts.Int# ->                    -- token number
         Happy_GHC_Exts.Int# ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 1# tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 1# tk st sts stk
     = happyFail [] 1# tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 1# tk st sts stk
     = happyFail [] 1# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 1# tk st sts stk
     = happyFail [] 1# tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 1# tk st sts stk
     = happyFail [] 1# tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 1# tk st sts stk
     = happyFail [] 1# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn 1# tk st sts stk
     = happyFail [] 1# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 1# tk st sts stk
     = happyFail [] 1# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Happy_GHC_Exts.Int#
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n ((_):(t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 1# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (Happy_GHC_Exts.I# (i)) -> i }) in
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
        action 1# 1# tk (HappyState (action)) sts ((HappyErrorToken (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


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
