{-
 - Encode Mk 2, using a prefix table for the codes
 - 
 - Paul Sanders, Systems Research, British Telecom Laboratories 1992
 -
 -}

module Encode (encode) where

import Defaults
import PTTrees

-- for convenience we make the code table type explicit

type CodeTable = PrefixTree Char Int

{- encode sets up the arguments for the real function.
 -}

encode :: String -> [Int]
encode input
          = encode' input first_code initial_table

{- encode' loops through the input string assembling the codes produced
 - by code_string. 
 - The first character is treated specially in that it is not added to the
 - table; its code is simply its ascii value
 -}

-- implicit selections with IMPLICIT DW

--#define IMPLICIT

-- explicit selections with EXPLICIT DW

#define EXPLICIT

--encode' :: String -> Int -> CodeTable -> [Int]
encode' [] _ _ 
	= []
encode' input v t
	= case (code_string input 0 v t) of { (input', n, t') ->
	  n : encode' input' (v + 1) t'
	  }
{- ???
encode' input v t
	= n : encode' input' (v + 1) t'
          where (input', n, t') = code_string input 0 v t
-}

{- code_string parses enough of the input string to produce one code and
 - returns the remaining input, the code and a new code table.
 -
 - The first character is taken and its place found in the code table. The
 - extension code table found for this character is then used as the lookup
 - table for the next character.
 -
 - If a character is not found in the current table then output the code
 - of the character associated with the current table and add the current
 - character to the current table and assign it the next new code value.
 -}


#ifdef IMPLICIT
code_string :: String -> Int -> Int -> CodeTable -> (String, Int, CodeTable)
code_string [] old_code _ _
	= ([], old_code, PTNil)
code_string i@(c:cs) old_code next_code PTNil   -- found max string, add suffix
	= if next_code >= max_entries
          then (i, old_code, PTNil)
          else (i, old_code, PT (PTE c next_code PTNil) PTNil PTNil)
code_string i@(c:cs) old_code next_code (PT (PTE k v t) l r)
	| c < k  = (csl, nl, PT (PTE k v t) l' r)
	| c > k  = (csr, nr, PT (PTE k v t) l r')
	| c == k = (cs', n, PT (PTE k v t') l r)
        where (csl, nl, l') = code_string i old_code next_code l
              (csr, nr, r') = code_string i old_code next_code r
              (cs', n, t') = code_string cs v next_code t
#endif

#ifdef EXPLICIT
{- ???
code_string :: String -> Int -> Int -> CodeTable -> (String, Int, CodeTable)
code_string [] old_code _ _
	= ([], old_code, PTNil)
code_string ca@(c:_) old_code next_code PTNil = -- found max string, add suffix
	if next_code >= max_entries then 
	    (ca, old_code, PTNil)
	else 
	    (ca, old_code, PT (PTE c next_code PTNil) PTNil PTNil)
-}
{- partain:ORIG:
code_string ca@(c:_) old_code next_code (PT p@(PTE k _ _) l r)
	| c < k  = f1 result1 p r
        where result1 = code_string ca old_code next_code l
	      
code_string ca@(c:_) old_code next_code (PT p@(PTE k _ _) l r)
	| c > k  = f2 result2 p l 
        where result2 = code_string ca old_code next_code r
	
code_string (_:cs) old_code next_code (PT (PTE k v t) l r)
	| otherwise = f3 result3 k v l r
        where result3 = code_string cs v next_code t

f1 (csl,nl,l') p r = (csl, nl, PT p l' r)
f2 (csr,nr,r') p l = (csr, nr, PT p l r')
f3 (cs',n,t') k v l r = (cs', n, PT (PTE k v t') l r)
-}
	
{- ???
code_string ca@(c:cs) old_code next_code (PT p@(PTE k v t) l r)
	| c < k  = f1 result1 p r
	| c > k  = f2 result2 p l 
	| otherwise = f3 result3 k v l r
	where
         result1 = code_string ca old_code next_code l
         result2 = code_string ca old_code next_code r
         result3 = code_string cs v next_code t

	 f1 (csl,nl,l') p r = (csl, nl, PT p l' r)
	 f2 (csr,nr,r') p l = (csr, nr, PT p l r')
	 f3 (cs',n,t') k v l r = (cs', n, PT (PTE k v t') l r)
-}

#define CBOX(c) (c)
#define _PTE_(a,b,c) (PTE (a) (b) (c))
#define _TRIP_(a,b,c) (a,b,c)
#define ILIT(i) (i)
#define _GE_ >=

code_string input@(CBOX(c) : input2) old_code next_code (PT p@(PTE k v t) l r)
    | CBOX(c) <  CBOX(k) = {-_scc_ "cs1"-} (f1 r1 p r)
    | CBOX(c) >  CBOX(k) = {-_scc_ "cs2"-} (f2 r2 p l)
    | otherwise {- CBOX(c) == CBOX(k) -} = {-_scc_ "cs3"-} (f3 r3 k v l r)
    where {
        r1 = code_string input old_code next_code l;
        r2 = code_string input old_code next_code r;
        r3 = code_string input2 v next_code t;

        f1 _TRIP_(input_l,nl,l2) p r   = _TRIP_(input_l,nl,PT p l2 r);
        f2 _TRIP_(input_r,nr,r2) p l   = _TRIP_(input_r,nr,PT p l r2);
        f3 _TRIP_(input2,n,t2) k v l r = _TRIP_(input2, n, PT _PTE_(k, v, t2) l r);
    }

code_string input@(CBOX(c) : input_file2) old_code next_code PTNil
    =   if (next_code _GE_ ILIT(4096)) 
        then {- _scc_ "cs4"-} _TRIP_(input, old_code, PTNil)
        else {- _scc_ "cs5"-} _TRIP_(input, old_code, PT _PTE_(c, next_code, PTNil) PTNil PTNil)

code_string [] old_code next_code code_table = {-_scc_ "cs6"-} _TRIP_([], old_code, PTNil)

#endif

{- We want the inital table to be balanced, but this is expensive to compute
 - as a rebalance is needed evert two inserts (yuk!). So we do the ordinary
 - infix-order binary tree insert but give the keys in such an order as to
 - give a balanced tree.
 -
 - (I would have defined the tree by hand but the constant was too big
 -  for hc-0.41)
 -}

initial_table :: CodeTable
initial_table
    = foldr tab_insert PTNil balanced_list

tab_insert n = insert (chr n) n

balanced_list
    = [128,64,32,16,8,4,2,1,0,3,6,5,7,12,10,9,11,14,13,15,24,20,18,17,19,22,
       21,23,28,26,25,27,30,29,31,48,40,36,34,33,35,38,37,39,44,42,41,43,46,
       45,47,56,52,50,49,51,54,53,55,60,58,57,59,62,61,63,96,80,72,68,66,65]
      ++ bal_list2 ++ bal_list3 ++ bal_list4 ++ bal_list5

bal_list2
    = [67,70,69,71,76,74,73,75,78,77,79,88,84,82,81,83,86,85,87,92,90,89,91,
       94,93,95,112,104,100,98,97,99,102,101,103,108,106,105,107,110,109,111,
       120,116,114,113,115,118,117,119,124,122,121,123,126,125,127,192,160]

bal_list3
    = [144,136,132,130,129,131,134,133,135,140,138,137,139,142,141,143,152,
       148,146,145,147,150,149,151,156,154,153,155,158,157,159,176,168,164,
       162,161,163,166,165,167,172,170,169,171,174,173,175,184,180,178,177]

bal_list4
    = [179,182,181,183,188,186,185,187,190,189,191,224,208,200,196,194,193,
       195,198,197,199,204,202,201,203,206,205,207,216,212,210,209,211,214,
       213,215,220,218,217,219,222,221,223,240,232,228,226,225,227,230,229,
       231,236,234,233,235,238,237,239,248,244,242,241,243,246,245,247,252]
bal_list5
    = [250,249,251,254,253,255]

