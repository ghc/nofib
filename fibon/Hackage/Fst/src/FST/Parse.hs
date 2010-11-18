-- parser produced by Happy Version 1.10

module FST.Parse where

import FST.NReg
import FST.RRegTypes (RReg)
import FST.Lexer

import Control.Monad (liftM,liftM2)

data HappyAbsSyn t5 t6 t7
	= HappyTerminal Token
	| HappyErrorToken Int
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7

action_0 (29) = happyShift action_5
action_0 (30) = happyShift action_6
action_0 (5) = happyGoto action_19
action_0 (6) = happyGoto action_4
action_0 _ = happyFail

action_1 (9) = happyShift action_8
action_1 (11) = happyShift action_9
action_1 (13) = happyShift action_10
action_1 (16) = happyShift action_11
action_1 (17) = happyShift action_12
action_1 (21) = happyShift action_13
action_1 (22) = happyShift action_14
action_1 (23) = happyShift action_15
action_1 (29) = happyShift action_16
action_1 (32) = happyShift action_17
action_1 (33) = happyShift action_18
action_1 (7) = happyGoto action_7
action_1 _ = happyFail

action_2 (29) = happyShift action_5
action_2 (30) = happyShift action_6
action_2 (5) = happyGoto action_3
action_2 (6) = happyGoto action_4
action_2 _ = happyFail

action_3 (29) = happyShift action_5
action_3 (30) = happyShift action_6
action_3 (6) = happyGoto action_20
action_3 _ = happyFail

action_4 _ = happyReduce_3

action_5 (31) = happyShift action_36
action_5 _ = happyFail

action_6 (31) = happyShift action_35
action_6 _ = happyFail

action_7 (9) = happyShift action_8
action_7 (11) = happyShift action_9
action_7 (13) = happyShift action_10
action_7 (14) = happyShift action_27
action_7 (15) = happyShift action_28
action_7 (16) = happyShift action_11
action_7 (17) = happyShift action_12
action_7 (18) = happyShift action_29
action_7 (19) = happyShift action_30
action_7 (20) = happyShift action_31
action_7 (21) = happyShift action_13
action_7 (22) = happyShift action_14
action_7 (23) = happyShift action_15
action_7 (25) = happyShift action_32
action_7 (26) = happyShift action_33
action_7 (27) = happyShift action_34
action_7 (29) = happyShift action_16
action_7 (32) = happyShift action_17
action_7 (33) = happyShift action_18
action_7 (34) = happyAccept
action_7 (7) = happyGoto action_26
action_7 _ = happyFail

action_8 (9) = happyShift action_8
action_8 (11) = happyShift action_9
action_8 (13) = happyShift action_10
action_8 (16) = happyShift action_11
action_8 (17) = happyShift action_12
action_8 (21) = happyShift action_13
action_8 (22) = happyShift action_14
action_8 (23) = happyShift action_15
action_8 (29) = happyShift action_16
action_8 (32) = happyShift action_17
action_8 (33) = happyShift action_18
action_8 (7) = happyGoto action_25
action_8 _ = happyFail

action_9 (9) = happyShift action_8
action_9 (11) = happyShift action_9
action_9 (13) = happyShift action_10
action_9 (16) = happyShift action_11
action_9 (17) = happyShift action_12
action_9 (21) = happyShift action_13
action_9 (22) = happyShift action_14
action_9 (23) = happyShift action_15
action_9 (29) = happyShift action_16
action_9 (32) = happyShift action_17
action_9 (33) = happyShift action_18
action_9 (7) = happyGoto action_24
action_9 _ = happyFail

action_10 _ = happyReduce_10

action_11 (9) = happyShift action_8
action_11 (11) = happyShift action_9
action_11 (13) = happyShift action_10
action_11 (16) = happyShift action_11
action_11 (17) = happyShift action_12
action_11 (21) = happyShift action_13
action_11 (22) = happyShift action_14
action_11 (23) = happyShift action_15
action_11 (29) = happyShift action_16
action_11 (32) = happyShift action_17
action_11 (33) = happyShift action_18
action_11 (7) = happyGoto action_23
action_11 _ = happyFail

action_12 (9) = happyShift action_8
action_12 (11) = happyShift action_9
action_12 (13) = happyShift action_10
action_12 (16) = happyShift action_11
action_12 (17) = happyShift action_12
action_12 (21) = happyShift action_13
action_12 (22) = happyShift action_14
action_12 (23) = happyShift action_15
action_12 (29) = happyShift action_16
action_12 (32) = happyShift action_17
action_12 (33) = happyShift action_18
action_12 (7) = happyGoto action_22
action_12 _ = happyFail

action_13 _ = happyReduce_21

action_14 _ = happyReduce_22

action_15 (24) = happyShift action_21
action_15 _ = happyReduce_24

action_16 _ = happyReduce_6

action_17 _ = happyReduce_25

action_18 _ = happyReduce_26

action_19 (29) = happyShift action_5
action_19 (30) = happyShift action_6
action_19 (34) = happyAccept
action_19 (6) = happyGoto action_20
action_19 _ = happyFail

action_20 _ = happyReduce_2

action_21 (23) = happyShift action_47
action_21 _ = happyFail

action_22 (7) = happyGoto action_26
action_22 _ = happyReduce_17

action_23 (7) = happyGoto action_26
action_23 _ = happyReduce_16

action_24 (9) = happyShift action_8
action_24 (11) = happyShift action_9
action_24 (12) = happyShift action_46
action_24 (13) = happyShift action_10
action_24 (14) = happyShift action_27
action_24 (15) = happyShift action_28
action_24 (16) = happyShift action_11
action_24 (17) = happyShift action_12
action_24 (18) = happyShift action_29
action_24 (19) = happyShift action_30
action_24 (20) = happyShift action_31
action_24 (21) = happyShift action_13
action_24 (22) = happyShift action_14
action_24 (23) = happyShift action_15
action_24 (25) = happyShift action_32
action_24 (26) = happyShift action_33
action_24 (27) = happyShift action_34
action_24 (29) = happyShift action_16
action_24 (32) = happyShift action_17
action_24 (33) = happyShift action_18
action_24 (7) = happyGoto action_26
action_24 _ = happyFail

action_25 (9) = happyShift action_8
action_25 (10) = happyShift action_45
action_25 (11) = happyShift action_9
action_25 (13) = happyShift action_10
action_25 (14) = happyShift action_27
action_25 (15) = happyShift action_28
action_25 (16) = happyShift action_11
action_25 (17) = happyShift action_12
action_25 (18) = happyShift action_29
action_25 (19) = happyShift action_30
action_25 (20) = happyShift action_31
action_25 (21) = happyShift action_13
action_25 (22) = happyShift action_14
action_25 (23) = happyShift action_15
action_25 (25) = happyShift action_32
action_25 (26) = happyShift action_33
action_25 (27) = happyShift action_34
action_25 (29) = happyShift action_16
action_25 (32) = happyShift action_17
action_25 (33) = happyShift action_18
action_25 (7) = happyGoto action_26
action_25 _ = happyFail

action_26 (14) = happyShift action_27
action_26 (15) = happyShift action_28
action_26 (16) = happyShift action_11
action_26 (17) = happyShift action_12
action_26 (27) = happyShift action_34
action_26 (7) = happyGoto action_26
action_26 _ = happyReduce_18

action_27 _ = happyReduce_19

action_28 _ = happyReduce_20

action_29 (9) = happyShift action_8
action_29 (11) = happyShift action_9
action_29 (13) = happyShift action_10
action_29 (16) = happyShift action_11
action_29 (17) = happyShift action_12
action_29 (21) = happyShift action_13
action_29 (22) = happyShift action_14
action_29 (23) = happyShift action_15
action_29 (29) = happyShift action_16
action_29 (32) = happyShift action_17
action_29 (33) = happyShift action_18
action_29 (7) = happyGoto action_44
action_29 _ = happyFail

action_30 (9) = happyShift action_8
action_30 (11) = happyShift action_9
action_30 (13) = happyShift action_10
action_30 (16) = happyShift action_11
action_30 (17) = happyShift action_12
action_30 (21) = happyShift action_13
action_30 (22) = happyShift action_14
action_30 (23) = happyShift action_15
action_30 (29) = happyShift action_16
action_30 (32) = happyShift action_17
action_30 (33) = happyShift action_18
action_30 (7) = happyGoto action_43
action_30 _ = happyFail

action_31 (9) = happyShift action_8
action_31 (11) = happyShift action_9
action_31 (13) = happyShift action_10
action_31 (16) = happyShift action_11
action_31 (17) = happyShift action_12
action_31 (21) = happyShift action_13
action_31 (22) = happyShift action_14
action_31 (23) = happyShift action_15
action_31 (29) = happyShift action_16
action_31 (32) = happyShift action_17
action_31 (33) = happyShift action_18
action_31 (7) = happyGoto action_42
action_31 _ = happyFail

action_32 (9) = happyShift action_8
action_32 (11) = happyShift action_9
action_32 (13) = happyShift action_10
action_32 (16) = happyShift action_11
action_32 (17) = happyShift action_12
action_32 (21) = happyShift action_13
action_32 (22) = happyShift action_14
action_32 (23) = happyShift action_15
action_32 (29) = happyShift action_16
action_32 (32) = happyShift action_17
action_32 (33) = happyShift action_18
action_32 (7) = happyGoto action_41
action_32 _ = happyFail

action_33 (9) = happyShift action_8
action_33 (11) = happyShift action_9
action_33 (13) = happyShift action_10
action_33 (16) = happyShift action_11
action_33 (17) = happyShift action_12
action_33 (21) = happyShift action_13
action_33 (22) = happyShift action_14
action_33 (23) = happyShift action_15
action_33 (29) = happyShift action_16
action_33 (32) = happyShift action_17
action_33 (33) = happyShift action_18
action_33 (7) = happyGoto action_40
action_33 _ = happyFail

action_34 (28) = happyShift action_39
action_34 _ = happyFail

action_35 (9) = happyShift action_8
action_35 (11) = happyShift action_9
action_35 (13) = happyShift action_10
action_35 (16) = happyShift action_11
action_35 (17) = happyShift action_12
action_35 (21) = happyShift action_13
action_35 (22) = happyShift action_14
action_35 (23) = happyShift action_15
action_35 (29) = happyShift action_16
action_35 (32) = happyShift action_17
action_35 (33) = happyShift action_18
action_35 (7) = happyGoto action_38
action_35 _ = happyFail

action_36 (9) = happyShift action_8
action_36 (11) = happyShift action_9
action_36 (13) = happyShift action_10
action_36 (16) = happyShift action_11
action_36 (17) = happyShift action_12
action_36 (21) = happyShift action_13
action_36 (22) = happyShift action_14
action_36 (23) = happyShift action_15
action_36 (29) = happyShift action_16
action_36 (32) = happyShift action_17
action_36 (33) = happyShift action_18
action_36 (7) = happyGoto action_37
action_36 _ = happyFail

action_37 (8) = happyShift action_49
action_37 (9) = happyShift action_8
action_37 (11) = happyShift action_9
action_37 (13) = happyShift action_10
action_37 (14) = happyShift action_27
action_37 (15) = happyShift action_28
action_37 (16) = happyShift action_11
action_37 (17) = happyShift action_12
action_37 (18) = happyShift action_29
action_37 (19) = happyShift action_30
action_37 (20) = happyShift action_31
action_37 (21) = happyShift action_13
action_37 (22) = happyShift action_14
action_37 (23) = happyShift action_15
action_37 (25) = happyShift action_32
action_37 (26) = happyShift action_33
action_37 (27) = happyShift action_34
action_37 (29) = happyShift action_16
action_37 (32) = happyShift action_17
action_37 (33) = happyShift action_18
action_37 (7) = happyGoto action_26
action_37 _ = happyFail

action_38 (8) = happyShift action_48
action_38 (9) = happyShift action_8
action_38 (11) = happyShift action_9
action_38 (13) = happyShift action_10
action_38 (14) = happyShift action_27
action_38 (15) = happyShift action_28
action_38 (16) = happyShift action_11
action_38 (17) = happyShift action_12
action_38 (18) = happyShift action_29
action_38 (19) = happyShift action_30
action_38 (20) = happyShift action_31
action_38 (21) = happyShift action_13
action_38 (22) = happyShift action_14
action_38 (23) = happyShift action_15
action_38 (25) = happyShift action_32
action_38 (26) = happyShift action_33
action_38 (27) = happyShift action_34
action_38 (29) = happyShift action_16
action_38 (32) = happyShift action_17
action_38 (33) = happyShift action_18
action_38 (7) = happyGoto action_26
action_38 _ = happyFail

action_39 _ = happyReduce_9

action_40 (14) = happyShift action_27
action_40 (15) = happyShift action_28
action_40 (16) = happyShift action_11
action_40 (17) = happyShift action_12
action_40 (18) = happyShift action_29
action_40 (19) = happyShift action_30
action_40 (20) = happyShift action_31
action_40 (27) = happyShift action_34
action_40 (7) = happyGoto action_26
action_40 _ = happyReduce_7

action_41 (14) = happyShift action_27
action_41 (15) = happyShift action_28
action_41 (16) = happyShift action_11
action_41 (17) = happyShift action_12
action_41 (18) = happyShift action_29
action_41 (19) = happyShift action_30
action_41 (20) = happyShift action_31
action_41 (27) = happyShift action_34
action_41 (7) = happyGoto action_26
action_41 _ = happyReduce_8

action_42 (14) = happyShift action_27
action_42 (15) = happyShift action_28
action_42 (16) = happyShift action_11
action_42 (17) = happyShift action_12
action_42 (27) = happyShift action_34
action_42 (7) = happyGoto action_26
action_42 _ = happyReduce_13

action_43 (14) = happyShift action_27
action_43 (15) = happyShift action_28
action_43 (16) = happyShift action_11
action_43 (17) = happyShift action_12
action_43 (27) = happyShift action_34
action_43 (7) = happyGoto action_26
action_43 _ = happyReduce_15

action_44 (14) = happyShift action_27
action_44 (15) = happyShift action_28
action_44 (16) = happyShift action_11
action_44 (17) = happyShift action_12
action_44 (27) = happyShift action_34
action_44 (7) = happyGoto action_26
action_44 _ = happyReduce_14

action_45 _ = happyReduce_12

action_46 _ = happyReduce_11

action_47 _ = happyReduce_23

action_48 _ = happyReduce_4

action_49 _ = happyReduce_5

happyReduce_2 = happySpecReduce_2 5 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_2:happy_var_1
	)
happyReduction_2 _ _  = notHappyAtAll

happyReduce_3 = happySpecReduce_1 5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 ([happy_var_1]
	)
happyReduction_3 _  = notHappyAtAll

happyReduce_4 = happyReduce 4 6 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Main happy_var_3
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 4 6 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenFun  happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Function (fst $ snd happy_var_1) (snd $ snd happy_var_1) happy_var_3
	) `HappyStk` happyRest

happyReduce_6 = happyMonadReduce 1 7 happyReduction_6
happyReduction_6 ((HappyTerminal (TokenFun  happy_var_1)) `HappyStk`
	happyRest)
	 = happyThen ( case (parseList (snd $ snd happy_var_1) []) of
				      FailE str -> failE $ "\nfstStudio failed to parse.\nParse error at line: "++ show (fst happy_var_1) ++"\n"
				      Ok  list  -> returnE $ Fun (fst $ snd happy_var_1) list
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_7 = happySpecReduce_3 7 happyReduction_7
happyReduction_7 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (NComp happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll

happyReduce_8 = happySpecReduce_3 7 happyReduction_8
happyReduction_8 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (NCross happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll

happyReduce_9 = happySpecReduce_3 7 happyReduction_9
happyReduction_9 (HappyTerminal (TokenNum  happy_var_3))
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (foldr NProduct NEpsilon $ take ((snd happy_var_3)) $ repeat happy_var_1
	)
happyReduction_9 _ _ _  = notHappyAtAll

happyReduce_10 = happySpecReduce_1 7 happyReduction_10
happyReduction_10 (HappyTerminal (TokenConcatS happy_var_1))
	 =  HappyAbsSyn7
		 (foldr NProduct NEpsilon $ map (NSymbol.(:[])) (snd happy_var_1)
	)
happyReduction_10 _  = notHappyAtAll

happyReduce_11 = happySpecReduce_3 7 happyReduction_11
happyReduction_11 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (NUnion NEpsilon happy_var_2
	)
happyReduction_11 _ _ _  = notHappyAtAll

happyReduce_12 = happySpecReduce_3 7 happyReduction_12
happyReduction_12 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_12 _ _ _  = notHappyAtAll

happyReduce_13 = happySpecReduce_3 7 happyReduction_13
happyReduction_13 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (NUnion happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll

happyReduce_14 = happySpecReduce_3 7 happyReduction_14
happyReduction_14 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (NIntersect happy_var_1 (NComplement happy_var_3)
	)
happyReduction_14 _ _ _  = notHappyAtAll

happyReduce_15 = happySpecReduce_3 7 happyReduction_15
happyReduction_15 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (NIntersect happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll

happyReduce_16 = happySpecReduce_2 7 happyReduction_16
happyReduction_16 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (NComplement happy_var_2
	)
happyReduction_16 _ _  = notHappyAtAll

happyReduce_17 = happySpecReduce_2 7 happyReduction_17
happyReduction_17 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (NProduct (NProduct (NStar NAll) happy_var_2) (NStar NAll)
	)
happyReduction_17 _ _  = notHappyAtAll

happyReduce_18 = happySpecReduce_2 7 happyReduction_18
happyReduction_18 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (NProduct happy_var_1 happy_var_2
	)
happyReduction_18 _ _  = notHappyAtAll

happyReduce_19 = happySpecReduce_2 7 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (NStar happy_var_1
	)
happyReduction_19 _ _  = notHappyAtAll

happyReduce_20 = happySpecReduce_2 7 happyReduction_20
happyReduction_20 _
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (NProduct happy_var_1  (NStar happy_var_1 )
	)
happyReduction_20 _ _  = notHappyAtAll

happyReduce_21 = happySpecReduce_1 7 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn7
		 (NEpsilon
	)

happyReduce_22 = happySpecReduce_1 7 happyReduction_22
happyReduction_22 _
	 =  HappyAbsSyn7
		 (NAll
	)

happyReduce_23 = happySpecReduce_3 7 happyReduction_23
happyReduction_23 (HappyTerminal (TokenS happy_var_3))
	_
	(HappyTerminal (TokenS happy_var_1))
	 =  HappyAbsSyn7
		 (NRelation (snd happy_var_1) (snd happy_var_3)
	)
happyReduction_23 _ _ _  = notHappyAtAll

happyReduce_24 = happySpecReduce_1 7 happyReduction_24
happyReduction_24 (HappyTerminal (TokenS happy_var_1))
	 =  HappyAbsSyn7
		 (NSymbol $ snd happy_var_1
	)
happyReduction_24 _  = notHappyAtAll

happyReduce_25 = happySpecReduce_1 7 happyReduction_25
happyReduction_25 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn7
		 (NVar $ snd happy_var_1
	)
happyReduction_25 _  = notHappyAtAll

happyReduce_26 = happyMonadReduce 1 7 happyReduction_26
happyReduction_26 ((HappyTerminal (Err happy_var_1)) `HappyStk`
	happyRest)
	 = happyThen ( failE happy_var_1
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyNewToken action sts stk [] =
	action 34 34 (error "reading EOF!") (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenSemi happy_dollar_dollar -> cont 8;
	TokenHOB happy_dollar_dollar -> cont 9;
	TokenHCB happy_dollar_dollar -> cont 10;
	TokenSOB happy_dollar_dollar -> cont 11;
	TokenSCB happy_dollar_dollar -> cont 12;
	TokenConcatS happy_dollar_dollar -> cont 13;
	TokenStar happy_dollar_dollar -> cont 14;
	TokenPlus happy_dollar_dollar -> cont 15;
	TokenComplement happy_dollar_dollar -> cont 16;
	TokenContainment happy_dollar_dollar -> cont 17;
	TokenMinus happy_dollar_dollar -> cont 18;
	TokenIntersect happy_dollar_dollar -> cont 19;
	TokenUnion happy_dollar_dollar -> cont 20;
	TokenEps happy_dollar_dollar -> cont 21;
	TokenAll happy_dollar_dollar -> cont 22;
	TokenS happy_dollar_dollar -> cont 23;
	TokenRelation happy_dollar_dollar -> cont 24;
	TokenCrossproduct happy_dollar_dollar -> cont 25;
	TokenComposition happy_dollar_dollar -> cont 26;
	TokenRepeat happy_dollar_dollar -> cont 27;
	TokenNum  happy_dollar_dollar -> cont 28;
	TokenFun  happy_dollar_dollar -> cont 29;
	TokenMain happy_dollar_dollar -> cont 30;
	TokenDef  happy_dollar_dollar -> cont 31;
	TokenVar happy_dollar_dollar -> cont 32;
	Err happy_dollar_dollar -> cont 33;
	}

happyThen = (thenE)
happyReturn = (returnE)
happyThen1 m k tks = (thenE) m (\a -> k a tks)
happyReturn1 = \a tks -> (returnE) a

parse tks = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn5 z -> happyReturn z; _other -> notHappyAtAll })

parseNReg tks = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn7 z -> happyReturn z; _other -> notHappyAtAll })

happyError :: [Token] -> E a
happyError _ = failE $ "\nfstStudio failed to parse.\n No useful message can be printed.\n"

data E a =   Ok a
	   | FailE String

instance Monad (E) where
 return = returnE
 (>>=)  = thenE

m `thenE` k = case m of
	       Ok   a    -> k a
	       FailE str -> FailE str

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE str = FailE str

data Def  = Main (NReg String) |
            Function Name [String] (NReg String)

functional (Ok def)     = case (getMain def) of
                           Ok main   -> apply main def
                           FailE str -> FailE str
functional (FailE str)  = FailE str

getMain []             = failE "\nfstStudio failed to parse.\nNo main function exists.\n"
getMain ((Main n1):xs) = Ok n1
getMain (_:xs)         = getMain xs

apply :: NReg String -> [Def] -> E (NReg String)
apply  (NCross n1 n2)     env = do liftM2 NCross      (apply n1 env) (apply n2 env)
apply  (NComp  n1 n2)     env = do liftM2 NComp       (apply n1 env) (apply n2 env)
apply  (NUnion n1 n2)     env = do liftM2 NUnion      (apply n1 env) (apply n2 env)
apply  (NProduct n1 n2)   env = do liftM2 NProduct    (apply n1 env) (apply n2 env)
apply  (NIntersect n1 n2) env = do liftM2 NIntersect  (apply n1 env) (apply n2 env)
apply  (NStar n1)         env = do liftM  NStar       (apply n1 env)
apply  (NComplement n1)   env = do liftM  NComplement (apply n1 env)
apply  (Fun str ns)       env = do applyFun (str,ns) env env
apply  n1                 _   = returnE n1

applyFun (str,_) []   _      = failE $ "\nfstStudio failed to parse.\nFound a unidentified function: " ++ str ++ "\n"
applyFun (str,ns) ((Function name vars n1):xs) env
 | str == name               = do res <- (replace n1 (zip vars ns))
				  apply res env
 | otherwise                 = do applyFun (str,ns) xs env
applyFun (str,ns) (_:xs) env = do applyFun (str,ns) xs env

replace :: NReg String -> [(String,NReg String)] -> E (NReg String)
replace (NCross n1 n2)      env  = do liftM2 NCross      (replace n1 env) (replace n2 env)
replace (NComp n1 n2)       env  = do liftM2 NComp       (replace n1 env) (replace n2 env)
replace (NUnion n1 n2)      env  = do liftM2 NUnion      (replace n1 env) (replace n2 env)
replace (NProduct n1 n2)    env  = do liftM2 NProduct    (replace n1 env) (replace n2 env)
replace (NStar n1)          env  = do liftM  NStar       (replace n1 env)
replace (NIntersect n1 n2)  env  = do liftM2 NIntersect  (replace n1 env) (replace n2 env)
replace (NComplement n1)    env  = do liftM NComplement  (replace n1 env)
replace (NVar str)          env  = case (lookup str env) of
				    Just n1 -> returnE n1
				    Nothing -> failE $ "\nfstStudio failed to parse.\nFound a unidentified variable: " ++ str ++"\n"
replace n1                  env  = returnE n1

parseList :: [String] -> [NReg String] -> E ([NReg String])
parseList [] res         = Ok (reverse res)
parseList (str:list) res = case ((parseNReg.lexer) str) of
			    FailE str -> FailE str
			    Ok n1     -> parseList list (n1:res)

parseExp :: String -> Either String (RReg String)
parseExp str = case ((parseNReg.lexer) str) of
                FailE str -> Left "\nfstStudio failed to parse given expression.\n"
                Ok n1 -> case (toRReg (nVarToSymbol n1)) of
                          Just rreg -> Right rreg
                          Nothing -> Left "\nfstStudio failed to parse given expression.\n"

parseProgram :: String -> Either String (RReg String)
parseProgram str = case ((functional.parse.lexer) str) of
                    FailE str -> Left str
                    Ok n1     -> case (toRReg n1) of
                                  Just rreg -> Right  rreg
                                  Nothing   -> Left "\nfstStudio failed to parse.\nNo main function exists.\n"
{-# LINE 1 "GenericTemplate.hs" -}
{-# LINE 1 "GenericTemplate.hs" -}
-- $Id: GenericTemplate.hs,v 1.11 2001/03/30 14:24:07 simonmar Exp $

{-# LINE 15 "GenericTemplate.hs" -}






















































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

happyAccept j tk st sts (HappyStk ans _) =

					   (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 127 "GenericTemplate.hs" -}


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
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
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = action nt j tk st sts (fn v1 `HappyStk` stk')

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = action nt j tk st sts (fn v1 v2 `HappyStk` stk')

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = action nt j tk st sts (fn v1 v2 v3 `HappyStk` stk')

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk = action nt j tk st1 sts1 (fn stk)
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - (1)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - (1)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (1) tk old_st _ stk =
--	trace "failing" $
    	happyError


{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts))
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







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
