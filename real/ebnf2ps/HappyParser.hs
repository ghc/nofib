-- parser produced by Happy Version 0.7 + PJT patch
-- Copyright 1994 by Peter Thiemann


module HappyParser (theHappyParser) where
import AbstractSyntax
import Lexer

data HappyAbsSyn 
	= HappyTerminal ( Token' )
	| HappyAbsSyn1 (  [Production]  )
	| HappyAbsSyn2 (  [Production]  )
	| HappyAbsSyn3 (  Production  )
	| HappyAbsSyn4 (  [Production]  )
	| HappyAbsSyn5 (  Production  )
	| HappyAbsSyn6 (  [Production]  )
	| HappyAbsSyn7 (  Production  )
	| HappyAbsSyn8 (  ()  )
	| HappyAbsSyn9 (  ()  )
	| HappyAbsSyn10 (  ()  )
	| HappyAbsSyn11 (  ()  )
	| HappyAbsSyn12 (  ()  )
	| HappyAbsSyn13 (  ()  )
	| HappyAbsSyn14 (  ()  )
	| HappyAbsSyn15 (  ()  )

type HappyReduction = 
	   Int 
	-> ( Token' ) 
	-> State ( Token' ) ([HappyAbsSyn] -> ( [Production] )) 
	-> Int 
	-> [ Token' ] 
	-> [State ( Token' ) ([HappyAbsSyn] -> ( [Production] ))] 
	-> [HappyAbsSyn] 
	-> ( [Production] )

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
 action_56 :: Int -> HappyReduction

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
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
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37 :: HappyReduction

action_0 23 = happyShift action_4
action_0 1 = happyGoto action_1
action_0 12 = happyGoto action_2
action_0 13 = happyGoto action_3
action_0 _ = happyReduce_24

action_1 27 = happyAccept
action_1 _ = happyFail 1

action_2 21 = happyShift action_19
action_2 8 = happyGoto action_17
action_2 9 = happyGoto action_18
action_2 _ = happyFail 2

action_3 _ = happyReduce_23

action_4 16 = happyShift action_8
action_4 17 = happyShift action_9
action_4 18 = happyShift action_10
action_4 19 = happyShift action_11
action_4 20 = happyShift action_12
action_4 21 = happyShift action_13
action_4 22 = happyShift action_14
action_4 23 = happyShift action_4
action_4 25 = happyShift action_15
action_4 26 = happyShift action_16
action_4 13 = happyGoto action_5
action_4 14 = happyGoto action_6
action_4 15 = happyGoto action_7
action_4 _ = happyFail 4

action_5 _ = happyReduce_30

action_6 24 = happyShift action_24
action_6 _ = happyFail 6

action_7 16 = happyShift action_8
action_7 17 = happyShift action_9
action_7 18 = happyShift action_10
action_7 19 = happyShift action_11
action_7 20 = happyShift action_12
action_7 21 = happyShift action_13
action_7 22 = happyShift action_14
action_7 23 = happyShift action_4
action_7 25 = happyShift action_15
action_7 26 = happyShift action_16
action_7 13 = happyGoto action_5
action_7 14 = happyGoto action_23
action_7 15 = happyGoto action_7
action_7 _ = happyReduce_27

action_8 _ = happyReduce_29

action_9 _ = happyReduce_32

action_10 _ = happyReduce_33

action_11 _ = happyReduce_34

action_12 _ = happyReduce_36

action_13 _ = happyReduce_37

action_14 _ = happyReduce_35

action_15 _ = happyReduce_31

action_16 _ = happyReduce_28

action_17 20 = happyShift action_22
action_17 _ = happyFail 17

action_18 21 = happyShift action_19
action_18 8 = happyGoto action_21
action_18 9 = happyGoto action_18
action_18 _ = happyReduce_15

action_19 16 = happyShift action_20
action_19 _ = happyFail 19

action_20 16 = happyShift action_31
action_20 23 = happyShift action_4
action_20 26 = happyShift action_32
action_20 10 = happyGoto action_28
action_20 11 = happyGoto action_29
action_20 13 = happyGoto action_30
action_20 _ = happyReduce_22

action_21 _ = happyReduce_14

action_22 16 = happyShift action_27
action_22 2 = happyGoto action_25
action_22 3 = happyGoto action_26
action_22 _ = happyFail 22

action_23 _ = happyReduce_26

action_24 _ = happyReduce_25

action_25 16 = happyShift action_27
action_25 23 = happyShift action_4
action_25 3 = happyGoto action_37
action_25 12 = happyGoto action_38
action_25 13 = happyGoto action_3
action_25 _ = happyReduce_24

action_26 _ = happyReduce_3

action_27 17 = happyShift action_35
action_27 19 = happyShift action_36
action_27 _ = happyFail 27

action_28 _ = happyReduce_16

action_29 _ = happyReduce_19

action_30 _ = happyReduce_17

action_31 23 = happyShift action_4
action_31 13 = happyGoto action_34
action_31 _ = happyReduce_18

action_32 23 = happyShift action_4
action_32 13 = happyGoto action_33
action_32 _ = happyFail 32

action_33 16 = happyShift action_47
action_33 26 = happyShift action_32
action_33 11 = happyGoto action_48
action_33 _ = happyReduce_22

action_34 16 = happyShift action_47
action_34 26 = happyShift action_32
action_34 11 = happyGoto action_46
action_34 _ = happyReduce_22

action_35 16 = happyShift action_44
action_35 26 = happyShift action_45
action_35 4 = happyGoto action_40
action_35 5 = happyGoto action_41
action_35 6 = happyGoto action_42
action_35 7 = happyGoto action_43
action_35 _ = happyReduce_11

action_36 23 = happyShift action_4
action_36 13 = happyGoto action_39
action_36 _ = happyFail 36

action_37 _ = happyReduce_2

action_38 _ = happyReduce_1

action_39 16 = happyShift action_52
action_39 _ = happyFail 39

action_40 _ = happyReduce_5

action_41 22 = happyShift action_51
action_41 _ = happyReduce_7

action_42 23 = happyShift action_4
action_42 13 = happyGoto action_50
action_42 _ = happyFail 42

action_43 16 = happyShift action_44
action_43 26 = happyShift action_45
action_43 6 = happyGoto action_49
action_43 7 = happyGoto action_43
action_43 _ = happyReduce_11

action_44 _ = happyReduce_13

action_45 _ = happyReduce_12

action_46 _ = happyReduce_20

action_47 23 = happyShift action_4
action_47 13 = happyGoto action_34
action_47 _ = happyFail 47

action_48 _ = happyReduce_21

action_49 _ = happyReduce_10

action_50 18 = happyShift action_55
action_50 _ = happyReduce_9

action_51 16 = happyShift action_44
action_51 26 = happyShift action_45
action_51 4 = happyGoto action_54
action_51 5 = happyGoto action_41
action_51 6 = happyGoto action_42
action_51 7 = happyGoto action_43
action_51 _ = happyReduce_11

action_52 17 = happyShift action_53
action_52 _ = happyFail 52

action_53 16 = happyShift action_44
action_53 26 = happyShift action_45
action_53 4 = happyGoto action_56
action_53 5 = happyGoto action_41
action_53 6 = happyGoto action_42
action_53 7 = happyGoto action_43
action_53 _ = happyReduce_11

action_54 _ = happyReduce_6

action_55 _ = happyReduce_8

action_56 _ = happyReduce_4

happyReduce_1 = happyReduce 1 5 reduction where {
  reduction (
	_ :
	HappyAbsSyn2  happy_var_4 :
	_ :
	_ :
	_ :
	happyRest)
	happy_var_lineno = HappyAbsSyn1
		(  reverse happy_var_4  ) : happyRest;
  reduction _ _ = notHappyAtAll 1}

happyReduce_2 = specHappyReduce_2 2 reduction where {
  reduction (
	HappyAbsSyn3  happy_var_2 :
	HappyAbsSyn2  happy_var_1 :
	happyRest)
	happy_var_lineno = HappyAbsSyn2
		(  happy_var_2 : happy_var_1  ) : happyRest;
  reduction _ _ = notHappyAtAll 2}

happyReduce_3 = specHappyReduce_1 2 reduction where {
  reduction (
	HappyAbsSyn3  happy_var_1 :
	happyRest)
	happy_var_lineno = HappyAbsSyn2
		(  [happy_var_1]  ) : happyRest;
  reduction _ _ = notHappyAtAll 3}

happyReduce_4 = happyReduce 3 6 reduction where {
  reduction (
	HappyAbsSyn4  happy_var_6 :
	_ :
	HappyTerminal happy_var_4 :
	_ :
	_ :
	_ :
	happyRest)
	happy_var_lineno = HappyAbsSyn3
		(  ProdProduction (getIdent' happy_var_4) [] (ProdTerm happy_var_6)  ) : happyRest;
  reduction _ _ = notHappyAtAll 4}

happyReduce_5 = specHappyReduce_3 3 reduction where {
  reduction (
	HappyAbsSyn4  happy_var_3 :
	_ :
	HappyTerminal happy_var_1 :
	happyRest)
	happy_var_lineno = HappyAbsSyn3
		(  ProdProduction (getIdent' happy_var_1) [] (ProdTerm happy_var_3)  ) : happyRest;
  reduction _ _ = notHappyAtAll 5}

happyReduce_6 = specHappyReduce_3 4 reduction where {
  reduction (
	HappyAbsSyn4  happy_var_3 :
	_ :
	HappyAbsSyn5  happy_var_1 :
	happyRest)
	happy_var_lineno = HappyAbsSyn4
		(  happy_var_1 : happy_var_3  ) : happyRest;
  reduction _ _ = notHappyAtAll 6}

happyReduce_7 = specHappyReduce_1 4 reduction where {
  reduction (
	HappyAbsSyn5  happy_var_1 :
	happyRest)
	happy_var_lineno = HappyAbsSyn4
		(  [happy_var_1]  ) : happyRest;
  reduction _ _ = notHappyAtAll 7}

happyReduce_8 = specHappyReduce_3 5 reduction where {
  reduction (
	_ :
	_ :
	HappyAbsSyn6  happy_var_1 :
	happyRest)
	happy_var_lineno = HappyAbsSyn5
		(  ProdFactor happy_var_1  ) : happyRest;
  reduction _ _ = notHappyAtAll 8}

happyReduce_9 = specHappyReduce_2 5 reduction where {
  reduction (
	_ :
	HappyAbsSyn6  happy_var_1 :
	happyRest)
	happy_var_lineno = HappyAbsSyn5
		(  ProdFactor happy_var_1  ) : happyRest;
  reduction _ _ = notHappyAtAll 9}

happyReduce_10 = specHappyReduce_2 6 reduction where {
  reduction (
	HappyAbsSyn6  happy_var_2 :
	HappyAbsSyn7  happy_var_1 :
	happyRest)
	happy_var_lineno = HappyAbsSyn6
		(  happy_var_1 : happy_var_2  ) : happyRest;
  reduction _ _ = notHappyAtAll 10}

happyReduce_11 = specHappyReduce_0 6 reduction where {
  reduction (
	happyRest)
	happy_var_lineno = HappyAbsSyn6
		(  []  ) : happyRest}

happyReduce_12 = specHappyReduce_1 7 reduction where {
  reduction (
	HappyTerminal happy_var_1 :
	happyRest)
	happy_var_lineno = HappyAbsSyn7
		(  ProdTerminal (getString' happy_var_1)  ) : happyRest;
  reduction _ _ = notHappyAtAll 12}

happyReduce_13 = specHappyReduce_1 7 reduction where {
  reduction (
	HappyTerminal happy_var_1 :
	happyRest)
	happy_var_lineno = HappyAbsSyn7
		(  ProdNonterminal (getIdent' happy_var_1)  ) : happyRest;
  reduction _ _ = notHappyAtAll 13}

happyReduce_14 = specHappyReduce_2 8 reduction where {
  reduction (
	_ :
	_ :
	happyRest)
	happy_var_lineno = HappyAbsSyn8
		(  ()  ) : happyRest;
  reduction _ _ = notHappyAtAll 14}

happyReduce_15 = specHappyReduce_1 8 reduction where {
  reduction (
	_ :
	happyRest)
	happy_var_lineno = HappyAbsSyn8
		(  ()  ) : happyRest;
  reduction _ _ = notHappyAtAll 15}

happyReduce_16 = specHappyReduce_3 9 reduction where {
  reduction (
	_ :
	_ :
	_ :
	happyRest)
	happy_var_lineno = HappyAbsSyn9
		(  ()  ) : happyRest;
  reduction _ _ = notHappyAtAll 16}

happyReduce_17 = specHappyReduce_1 10 reduction where {
  reduction (
	_ :
	happyRest)
	happy_var_lineno = HappyAbsSyn10
		(  ()  ) : happyRest;
  reduction _ _ = notHappyAtAll 17}

happyReduce_18 = specHappyReduce_1 10 reduction where {
  reduction (
	_ :
	happyRest)
	happy_var_lineno = HappyAbsSyn10
		(  ()  ) : happyRest;
  reduction _ _ = notHappyAtAll 18}

happyReduce_19 = specHappyReduce_1 10 reduction where {
  reduction (
	_ :
	happyRest)
	happy_var_lineno = HappyAbsSyn10
		(  ()  ) : happyRest;
  reduction _ _ = notHappyAtAll 19}

happyReduce_20 = specHappyReduce_3 11 reduction where {
  reduction (
	_ :
	_ :
	_ :
	happyRest)
	happy_var_lineno = HappyAbsSyn11
		(  ()  ) : happyRest;
  reduction _ _ = notHappyAtAll 20}

happyReduce_21 = specHappyReduce_3 11 reduction where {
  reduction (
	_ :
	_ :
	_ :
	happyRest)
	happy_var_lineno = HappyAbsSyn11
		(  ()  ) : happyRest;
  reduction _ _ = notHappyAtAll 21}

happyReduce_22 = specHappyReduce_0 11 reduction where {
  reduction (
	happyRest)
	happy_var_lineno = HappyAbsSyn11
		(  ()  ) : happyRest}

happyReduce_23 = specHappyReduce_1 12 reduction where {
  reduction (
	_ :
	happyRest)
	happy_var_lineno = HappyAbsSyn12
		(  ()  ) : happyRest;
  reduction _ _ = notHappyAtAll 23}

happyReduce_24 = specHappyReduce_0 12 reduction where {
  reduction (
	happyRest)
	happy_var_lineno = HappyAbsSyn12
		(  ()  ) : happyRest}

happyReduce_25 = specHappyReduce_3 13 reduction where {
  reduction (
	_ :
	_ :
	_ :
	happyRest)
	happy_var_lineno = HappyAbsSyn13
		(  ()  ) : happyRest;
  reduction _ _ = notHappyAtAll 25}

happyReduce_26 = specHappyReduce_2 14 reduction where {
  reduction (
	_ :
	_ :
	happyRest)
	happy_var_lineno = HappyAbsSyn14
		(  ()  ) : happyRest;
  reduction _ _ = notHappyAtAll 26}

happyReduce_27 = specHappyReduce_1 14 reduction where {
  reduction (
	_ :
	happyRest)
	happy_var_lineno = HappyAbsSyn14
		(  ()  ) : happyRest;
  reduction _ _ = notHappyAtAll 27}

happyReduce_28 = specHappyReduce_1 15 reduction where {
  reduction (
	_ :
	happyRest)
	happy_var_lineno = HappyAbsSyn15
		(  ()  ) : happyRest;
  reduction _ _ = notHappyAtAll 28}

happyReduce_29 = specHappyReduce_1 15 reduction where {
  reduction (
	_ :
	happyRest)
	happy_var_lineno = HappyAbsSyn15
		(  ()  ) : happyRest;
  reduction _ _ = notHappyAtAll 29}

happyReduce_30 = specHappyReduce_1 15 reduction where {
  reduction (
	_ :
	happyRest)
	happy_var_lineno = HappyAbsSyn15
		(  ()  ) : happyRest;
  reduction _ _ = notHappyAtAll 30}

happyReduce_31 = specHappyReduce_1 15 reduction where {
  reduction (
	_ :
	happyRest)
	happy_var_lineno = HappyAbsSyn15
		(  ()  ) : happyRest;
  reduction _ _ = notHappyAtAll 31}

happyReduce_32 = specHappyReduce_1 15 reduction where {
  reduction (
	_ :
	happyRest)
	happy_var_lineno = HappyAbsSyn15
		(  ()  ) : happyRest;
  reduction _ _ = notHappyAtAll 32}

happyReduce_33 = specHappyReduce_1 15 reduction where {
  reduction (
	_ :
	happyRest)
	happy_var_lineno = HappyAbsSyn15
		(  ()  ) : happyRest;
  reduction _ _ = notHappyAtAll 33}

happyReduce_34 = specHappyReduce_1 15 reduction where {
  reduction (
	_ :
	happyRest)
	happy_var_lineno = HappyAbsSyn15
		(  ()  ) : happyRest;
  reduction _ _ = notHappyAtAll 34}

happyReduce_35 = specHappyReduce_1 15 reduction where {
  reduction (
	_ :
	happyRest)
	happy_var_lineno = HappyAbsSyn15
		(  ()  ) : happyRest;
  reduction _ _ = notHappyAtAll 35}

happyReduce_36 = specHappyReduce_1 15 reduction where {
  reduction (
	_ :
	happyRest)
	happy_var_lineno = HappyAbsSyn15
		(  ()  ) : happyRest;
  reduction _ _ = notHappyAtAll 36}

happyReduce_37 = specHappyReduce_1 15 reduction where {
  reduction (
	_ :
	happyRest)
	happy_var_lineno = HappyAbsSyn15
		(  ()  ) : happyRest;
  reduction _ _ = notHappyAtAll 37}

happyNewToken action ln []
	= action 27 27 (error "reading EOF!") (HappyState action) ln []

happyNewToken action ln (tk:tks) = case tk of
	 Ident' _  -> cont 16
	 Colon  -> cont 17
	 Semicolon  -> cont 18
	 DoubleColon  -> cont 19
	 DoublePercent  -> cont 20
	 Percent  -> cont 21
	 Bar  -> cont 22
	 OpenBrace  -> cont 23
	 ClosingBrace  -> cont 24
	 Symbol' _  -> cont 25
	 String' _  -> cont 26
  where cont i = action i i tk (HappyState action) ln tks

localHappyParser = happyParse



happyError :: Int -> Int -> [Token'] -> a
happyError s i ts = error ("Parse error in line " ++ show i ++
                         " [state " ++ show s ++ "]" ++
                         case ts of
                         [] -> " (at EOF)\n"
                         _  ->  "\n" ++ show (take 20 ts) ++ "\n")



unlit :: String -> String
unlit = unlines . map (tail.tail) . filter p . lines
    where p ('>':' ':_)  = True
          p ('>':'\t':_) = True
          p _            = False



data Token' = Ident' String | Symbol' String | String' String
            | Percent | DoublePercent | OpenBrace | ClosingBrace
            | Colon | Semicolon | DoubleColon | Bar

instance Text Token' where
  showsPrec n (Ident' s) = showChar '[' . showString s . showString "] "
  showsPrec n (Symbol' s) = showChar '<' . showString s . showString "> "
  showsPrec n (String' s) = showChar '"' . showString s . showString "\" "
  showsPrec n Percent = showString "% "
  showsPrec n DoublePercent = showString "%% "
  showsPrec n OpenBrace  = showString "{ "
  showsPrec n ClosingBrace = showString "} "
  showsPrec n Colon = showString ": "
  showsPrec n Semicolon = showString "; "
  showsPrec n DoubleColon = showString ":: "


postlexer = map f
  where f (Symbol "%%") = DoublePercent
        f (Symbol "%")  = Percent
        f (Symbol "{")  = OpenBrace
        f (Symbol "}")  = ClosingBrace
        f (Symbol "::") = DoubleColon
        f (Symbol ":")  = Colon
        f (Symbol ";")  = Semicolon
        f (Symbol "|")  = Bar
        f (Symbol s)    = Symbol' s
        f (Ident  s)    = Ident' s
        f (String s)    = String' s

getIdent' (Ident' x) = x
getString' (String' x) = x

theHappyParser = localHappyParser . postlexer . lexer . unlit

-- Start of Happy Template (version 0.7)

happyParse tks = happyNewToken action_0 (1::Int) tks [] []

-- All this HappyState stuff is simply because we can't have recursive
-- types in Haskell without an intervening data structure.

data State b c = HappyState
	(Int ->				-- token number
	 Int ->				-- token number (yes, again)
	 b -> 				-- token semantic value
	 State b c ->			-- current state
	 Int ->				-- line number
	 [b] ->				-- rest of tokens
	 [State b c] ->			-- state stack
	 c)

-- Ok, Here are the action functions.

happyAccept _ _ _ _ _ _ [ HappyAbsSyn1 ans ] = ans

happyFail s _ _ _ ln tks _ _ = happyError s ln tks

happyShift new_state i tk st ln tks sts stk =
     happyNewToken new_state ln tks (st:sts) (HappyTerminal tk:stk)

happyGoto action j tk st = action j j tk (HappyState action)

-- happyReduce is specialised for the common cases.

specHappyReduce_0 i fn j tk st@(HappyState action) ln tks sts stk
     = action i j tk st ln tks (st:sts) (fn stk ln)
specHappyReduce_1 i fn j tk _ ln tks sts@(st@(HappyState action):_) stk
     = action i j tk st ln tks sts (fn stk ln)
specHappyReduce_2 i fn j tk _ ln tks (_:sts@(st@(HappyState action):_)) stk
     = action i j tk st ln tks sts (fn stk ln)
specHappyReduce_3 i fn j tk _ ln tks (_:_:sts@(st@(HappyState action):_)) stk
     = action i j tk st ln tks sts (fn stk ln)

happyReduce i k fn j tk st ln tks sts stk
              = action i j tk st' ln tks sts' (fn stk ln)
       where sts'@(st'@(HappyState action):_) = drop (k::Int) (st:sts)

-- Internal happy errors:

notHappyAtAll :: Int -> a
notHappyAtAll i = error ("Internal Happy error in reduction ( " 
			   ++ show i ++ " )")

-- end of Happy Template.
