--!!! simple tests of primitive arrays
--
module Main ( main ) where

import PreludeGlaST

main _ = [AppendChan stdout
	 (test_chars	++ "\n" ++
	  test_ints	++ "\n" ++
	  test_addrs	++ "\n" ++
	  test_floats	++ "\n" ++
	  test_doubles	++ "\n" ++
	  test_ptrs	++ "\n")
	 ]

-- Arr# Char# -------------------------------------------
-- (main effort is in packString#)

test_chars :: String
test_chars
  = let str = reverse "Now is the time for all good men to come to...\n"
    in
    unsafePerformPrimIO (
	_ccall_ fprintf (``stdout''::_Addr) "%d %s\n" 93 str `seqPrimIO`
	returnPrimIO ""
	)

-- Arr# Int# -------------------------------------------

test_ints :: String
test_ints
  = let arr# = f 1000
    in
	shows (lookup_range arr# 42# 416#) "\n"
  where
    f :: Int -> _ByteArray Int

    f size@(I# size#)
      = _runST (
	    -- allocate an array of the specified size
	  newIntArray (0, (size-1))	`thenST` \ arr# ->

	    -- fill in all elements; elem i has i^2 put in it
	  fill_in arr# 0# (size# `minusInt#` 1#) `seqST`

	    -- freeze the puppy:
	  freezeIntArray arr#
	)

    fill_in :: _MutableByteArray s Int -> Int# -> Int# -> _ST s ()

    fill_in arr_in# first# last#
      = if (first# ># last#)
	then returnST ()
	else writeIntArray arr_in# (I# first#) (I# (first# *# first#)) `seqST`
	     fill_in arr_in# (first# +# 1#) last#

    lookup_range :: _ByteArray Int -> Int# -> Int# -> [Int]
    lookup_range arr from# to#
      = if (from# ># to#)
	then []
	else (indexIntArray arr (I# from#))
	     : (lookup_range arr (from# +# 1#) to#)

-- Arr# Addr# -------------------------------------------

test_addrs :: String
test_addrs
  = let arr# = f 1000
    in
	shows (lookup_range arr# 42# 416#) "\n"
  where
    f :: Int -> _ByteArray Int

    f size@(I# size#)
      = _runST (
	    -- allocate an array of the specified size
	  newAddrArray (0, (size-1))	`thenST` \ arr# ->

	    -- fill in all elements; elem i has i^2 put in it
	  fill_in arr# 0# (size# `minusInt#` 1#) `seqST`

	    -- freeze the puppy:
	  freezeAddrArray arr#
	)

    fill_in :: _MutableByteArray s Int -> Int# -> Int# -> _ST s ()

    fill_in arr_in# first# last#
      = if (first# ># last#)
	then returnST ()
	else writeAddrArray arr_in# (I# first#)
			    (A# (int2Addr# (first# *# first#))) `seqST`
	     fill_in arr_in# (first# +# 1#) last#

    lookup_range :: _ByteArray Int -> Int# -> Int# -> [ Int ]
    lookup_range arr from# to#
      = let
	    a2i (A# a#) = I# (addr2Int# a#)
	in
	if (from# ># to#)
	then []
	else (a2i (indexAddrArray arr (I# from#)))
	     : (lookup_range arr (from# +# 1#) to#)

-- Arr# Float# -------------------------------------------

test_floats :: String
test_floats
  = let arr# = f 1000
    in
	shows (lookup_range arr# 42# 416#) "\n"
  where
    f :: Int -> _ByteArray Int

    f size@(I# size#)
      = _runST (
	    -- allocate an array of the specified size
	  newFloatArray (0, (size-1))	`thenST` \ arr# ->

	    -- fill in all elements; elem i has "i * pi" put in it
	  fill_in arr# 0# (size# `minusInt#` 1#) `seqST`

	    -- freeze the puppy:
	  freezeFloatArray arr#
	)

    fill_in :: _MutableByteArray s Int -> Int# -> Int# -> _ST s ()

    fill_in arr_in# first# last#
      = if (first# ># last#)
	then returnST ()
	else writeFloatArray arr_in# (I# first#) ((fromInt (I# first#)) * pi) `seqST`
	     fill_in arr_in# (first# +# 1#) last#

    lookup_range :: _ByteArray Int -> Int# -> Int# -> [Float]
    lookup_range arr from# to#
      = if (from# ># to#)
	then []
	else (indexFloatArray arr (I# from#))
	     : (lookup_range arr (from# +# 1#) to#)

-- Arr# Double# -------------------------------------------

test_doubles :: String
test_doubles
  = let arr# = f 1000
    in
	shows (lookup_range arr# 42# 416#) "\n"
  where
    f :: Int -> _ByteArray Int

    f size@(I# size#)
      = _runST (
	    -- allocate an array of the specified size
	  newDoubleArray (0, (size-1))	`thenST` \ arr# ->

	    -- fill in all elements; elem i has "i * pi" put in it
	  fill_in arr# 0# (size# `minusInt#` 1#) `seqST`

	    -- freeze the puppy:
	  freezeDoubleArray arr#
	)

    fill_in :: _MutableByteArray s Int -> Int# -> Int# -> _ST s ()

    fill_in arr_in# first# last#
      = if (first# ># last#)
	then returnST ()
	else writeDoubleArray arr_in# (I# first#) ((fromInt (I# first#)) * pi) `seqST`
	     fill_in arr_in# (first# +# 1#) last#

    lookup_range :: _ByteArray Int -> Int# -> Int# -> [Double]
    lookup_range arr from# to#
      = if (from# ># to#)
	then []
	else (indexDoubleArray arr (I# from#))
	     : (lookup_range arr (from# +# 1#) to#)

-- Arr# (Ratio Int) (ptrs) ---------------------------------
-- just like Int# test

test_ptrs :: String
test_ptrs
  = let arr# = f 1000
    in
	shows (lookup_range arr# 42 416) "\n"
  where
    f :: Int -> Array Int (Ratio Int)

    f size
      = _runST (
	  newArray (1, size) (3 % 5)	`thenST` \ arr# ->
	  -- don't fill in the whole thing
	  fill_in arr# 1 400		`seqST`
	  freezeArray arr#
	)

    fill_in :: _MutableArray s Int (Ratio Int) -> Int -> Int -> _ST s ()

    fill_in arr_in# first last
      = if (first > last)
	then returnST ()
	else writeArray arr_in# first (fromInt (first * first)) `seqST`
	     fill_in  arr_in# (first + 1) last

    lookup_range :: Array Int (Ratio Int) -> Int -> Int -> [Ratio Int]
    lookup_range array from too
      = if (from > too)
	then []
	else (array ! from) : (lookup_range array (from + 1) too)
