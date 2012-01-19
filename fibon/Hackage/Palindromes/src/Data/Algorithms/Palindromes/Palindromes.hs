-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Algorithms.Palindromes.Palindromes
-- Copyright   :  (c) 2007 - 2010 Johan Jeuring
-- License     :  BSD3
--
-- Maintainer  :  johan@jeuring.net
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Data.Algorithms.Palindromes.Palindromes
       (longestPalindrome
       ,longestPalindromes
       ,lengthLongestPalindrome
       ,lengthLongestPalindromes
       ,longestTextPalindrome
       ,longestTextPalindromes
       ,longestWordPalindrome
       ,longestWordPalindromes
       ,palindromesAroundCentres
       ) where
 
import Data.List (maximumBy,intersperse)
import Data.Char
import Data.Array 
 
import Control.Arrow
 
-- All functions in the interface, except palindromesAroundCentres 
-- have the type String -> String
       
-----------------------------------------------------------------------------
-- longestPalindrome
-----------------------------------------------------------------------------

-- | longestPalindrome returns the longest palindrome in a string.
longestPalindrome :: String -> String
longestPalindrome input = 
  let inputArray       =  listArrayl0 input
      (maxLength,pos)  =  maximumBy 
                            (\(l,_) (l',_) -> compare l l') 
                            (zip (palindromesAroundCentres inputArray) [0..])    
  in showPalindrome inputArray (maxLength,pos)

-----------------------------------------------------------------------------
-- longestPalindromes
-----------------------------------------------------------------------------

-- | longestPalindromes returns the longest palindrome around each position
--   in a string. The integer argument is used to only show palindromes
--   of length at least this integer.
longestPalindromes :: Int -> String -> String
longestPalindromes m input = 
  let inputArray       =  listArrayl0 input
  in    concat 
      $ intersperse "\n" 
      $ map (showPalindrome inputArray) 
      $ filter ((m<=) . fst)
      $ zip (palindromesAroundCentres inputArray) [0..]

-----------------------------------------------------------------------------
-- lengthLongestPalindrome
-----------------------------------------------------------------------------

-- | lengthLongestPalindrome returns the length of the longest palindrome in 
--   a string.
lengthLongestPalindrome :: String -> String
lengthLongestPalindrome =
  show . maximum . palindromesAroundCentres . listArrayl0

-----------------------------------------------------------------------------
-- lengthLongestPalindromes
-----------------------------------------------------------------------------

-- | lengthLongestPalindromes returns the lengths of the longest palindrome  
--   around each position in a string.
lengthLongestPalindromes :: String -> String
lengthLongestPalindromes =
  show . palindromesAroundCentres . listArrayl0

-----------------------------------------------------------------------------
-- longestTextPalindrome
-----------------------------------------------------------------------------

-- | longestTextPalindrome returns the longest text palindrome in a string,
--   ignoring spacing, punctuation symbols, and case of letters.
longestTextPalindrome :: String -> String
longestTextPalindrome input = 
  let inputArray              =  listArrayl0 input
      ips                     =  zip input [0..]
      textinput               =  map (first toLower) 
                                     (filter (isLetter.fst) ips)
      textInputArray          =  listArrayl0 (map fst textinput)
      positionTextInputArray  =  listArrayl0 (map snd textinput)
  in  longestTextPalindromeArray 
        textInputArray 
        positionTextInputArray 
        inputArray

longestTextPalindromeArray :: 
  (Show a, Eq a) => Array Int a -> Array Int Int -> Array Int a -> String
longestTextPalindromeArray a positionArray inputArray = 
  let (len,pos) = maximumBy 
                    (\(l,_) (l',_) -> compare l l') 
                    (zip (palindromesAroundCentres a) [0..])    
  in showTextPalindrome positionArray inputArray (len,pos) 

-----------------------------------------------------------------------------
-- longestTextPalindromes
-----------------------------------------------------------------------------

-- | longestTextPalindromes returns the longest text palindrome around each
--   position in a string. The integer argument is used to only show palindromes
--   of length at least this integer.
longestTextPalindromes :: Int -> String -> String
longestTextPalindromes m input = 
  let inputArray              =  listArrayl0 input
      ips                     =  zip input [0..]
      textinput               =  map (first toLower) 
                                     (filter (isLetter.fst) ips)
      textInputArray          =  listArrayl0 (map fst textinput)
      positionTextInputArray  =  listArrayl0 (map snd textinput)
  in  concat 
    $ intersperse "\n" 
    $ longestTextPalindromesArray 
        m
        textInputArray 
        positionTextInputArray 
        inputArray

longestTextPalindromesArray :: 
  (Show a, Eq a) => 
    Int -> Array Int a -> Array Int Int -> Array Int a -> [String]
longestTextPalindromesArray m a positionArray inputArray = 
    map (showTextPalindrome positionArray inputArray) 
  $ filter ((m<=) . fst)
  $ zip (palindromesAroundCentres a) [0..]

-----------------------------------------------------------------------------
-- longestWordPalindrome
-----------------------------------------------------------------------------

-- | longestWordPalindrome returns the longest text palindrome preceded and 
--   followed by punctuation symbols (if any). Note that if a word palindrome is 
--   accidentally surrounded by the same symbols, it won't be found. For 
--   example, the longest word palindrome in "w waaw wo waw" is "waw". We could 
--   change longestWordPalindrome to return the longest enclosed word 
--   palindrome, but that would give a quadratic time algorithm.
longestWordPalindrome :: String -> String
longestWordPalindrome input = 
  let inputArray              =  listArrayl0 input
      ips                     =  zip input [0..]
      textinput               =  map (first toLower) 
                                     (filter (isLetter.fst) ips)
      textInputArray          =  listArrayl0 (map fst textinput)
      positionTextInputArray  =  listArrayl0 (map snd textinput)
  in  longestWordPalindromeArray 
        textInputArray 
        positionTextInputArray 
        inputArray

longestWordPalindromeArray :: 
    Array Int Char -> Array Int Int -> Array Int Char -> String
longestWordPalindromeArray a positionArray inputArray = 
  let wordPalindromes = filter (isWordpalindrome positionArray inputArray)
                      $ zip (palindromesAroundCentres a) [0..]
  in  if null wordPalindromes 
      then ""
      else showTextPalindrome positionArray inputArray $
             maximumBy (\(l,_) (l',_) -> compare l l') wordPalindromes

-----------------------------------------------------------------------------
-- longestWordPalindromes
-----------------------------------------------------------------------------

-- | longestWordPalindromes returns the longest word palindrome around each
--   position in a string. The integer argument is used to only show 
--   palindromes of length at least this integer.
longestWordPalindromes :: Int -> String -> String
longestWordPalindromes m input = 
  let inputArray              =  listArrayl0 input
      ips                     =  zip input [0..]
      textinput               =  map (first toLower) 
                                     (filter (isLetter.fst) ips)
      textInputArray          =  listArrayl0 (map fst textinput)
      positionTextInputArray  =  listArrayl0 (map snd textinput)
  in  concat 
    $ intersperse "\n" 
    $ longestWordPalindromesArray 
        m
        textInputArray 
        positionTextInputArray 
        inputArray

longestWordPalindromesArray :: 
    Int -> Array Int Char -> Array Int Int -> Array Int Char -> [String]
longestWordPalindromesArray m a positionArray inputArray = 
    map (showTextPalindrome positionArray inputArray) 
  $ filter ((m<=) . fst)
  $ filter (isWordpalindrome positionArray inputArray)
  $ zip (palindromesAroundCentres a) [0..]

isWordpalindrome :: Array Int Int -> Array Int Char -> (Int,Int) -> Bool
isWordpalindrome positionArray inputArray (len,pos) = 
  let startpos   =  pos `div` 2 - len `div` 2
      endpos     =  if odd len 
                    then pos `div` 2 + len `div` 2 
                    else pos `div` 2 + len `div` 2 - 1
      startpos'  =  positionArray!!!startpos
      endpos'    =  positionArray!!!endpos
  in  if endpos < startpos
      then False
      else if startpos' <= fst (bounds inputArray)
           then endpos' >= snd (bounds inputArray) ||
                not (isLetter (inputArray!!!(endpos'+1)))
           else if endpos' >= snd (bounds inputArray) 
                then not (isLetter (inputArray!!!(startpos'-1)))
                else not (isLetter (inputArray!!!(startpos'-1)))
                  && not (isLetter (inputArray!!!(endpos'+1)))

-----------------------------------------------------------------------------
-- palindromesAroundCentres 
--
-- The function that implements the palindrome finding algorithm.
-- Used in all the interface functions.
-----------------------------------------------------------------------------

-- | palindromesAroundCentres is the central function of the module. It returns
--   the list of lenghths of the longest palindrome around each position in a
--   string.
palindromesAroundCentres    ::  (Eq a) => 
                                Array Int a -> [Int]
palindromesAroundCentres  a =   
  let (afirst,_) = bounds a
  in reverse $ extendTail a afirst 0 []

extendTail :: (Eq a) => 
              Array Int a -> Int -> Int -> [Int] -> [Int]
extendTail a n currentTail centres 
  | n > alast                    =  
      -- reached the end of the array                                     
      finalCentres currentTail centres 
                   (currentTail:centres)
  | n-currentTail == afirst      =  
      -- the current longest tail palindrome 
      -- extends to the start of the array
      extendCentres a n (currentTail:centres) 
                    centres currentTail 
  | a!n == a!(n-currentTail-1)   =  
      -- the current longest tail palindrome 
      -- can be extended
      extendTail a (n+1) (currentTail+2) centres      
  | otherwise                    =  
      -- the current longest tail palindrome 
      -- cannot be extended                 
      extendCentres a n (currentTail:centres) 
                    centres currentTail
  where  (afirst,alast)  =  bounds a

extendCentres :: (Eq a) =>
                 Array Int a -> Int -> [Int] -> [Int] -> Int -> [Int]
extendCentres a n centres tcentres centreDistance
  | centreDistance == 0                =  
      -- the last centre is on the last element: 
      -- try to extend the tail of length 1
      extendTail a (n+1) 1 centres
  | centreDistance-1 == head tcentres  =  
      -- the previous element in the centre list 
      -- reaches exactly to the end of the last 
      -- tail palindrome use the mirror property 
      -- of palindromes to find the longest tail 
      -- palindrome
      extendTail a n (head tcentres) centres
  | otherwise                          =  
      -- move the centres one step
      -- add the length of the longest palindrome 
      -- to the centres
      extendCentres a n (min (head tcentres) 
                    (centreDistance-1):centres) 
                    (tail tcentres) (centreDistance-1)

finalCentres :: Int -> [Int] -> [Int] -> [Int]
finalCentres n     _        _       | n < 0 =  error "finalCentres: input < 0"
finalCentres 0     _        centres  =  centres
finalCentres n tcentres centres  =
  finalCentres (n-1)
               (tail tcentres) 
               (min (head tcentres) (n-1):centres)

-----------------------------------------------------------------------------
-- Showing palindromes
-----------------------------------------------------------------------------

showPalindrome :: (Show a) => Array Int a -> (Int,Int) -> String
showPalindrome a (len,pos) = 
  let startpos = pos `div` 2 - len `div` 2
      endpos   = if odd len 
                 then pos `div` 2 + len `div` 2 
                 else pos `div` 2 + len `div` 2 - 1
  in show [a!n|n <- [startpos .. endpos]]

showTextPalindrome :: (Show a) => 
                      Array Int Int -> Array Int a -> (Int,Int) -> String
showTextPalindrome positionArray inputArray (len,pos) = 
  let startpos   =  pos `div` 2 - len `div` 2
      endpos     =  if odd len 
                    then pos `div` 2 + len `div` 2 
                    else pos `div` 2 + len `div` 2 - 1
  in  if endpos < startpos
      then []
      else let start      =  if startpos > fst (bounds positionArray)
                             then positionArray!!!(startpos-1)+1
                             else fst (bounds inputArray)
               end        =  if endpos < snd (bounds positionArray)
                             then positionArray!!!(endpos+1)-1
                             else snd (bounds inputArray) 
           in  show [inputArray!n | n<- [start..end]]

-----------------------------------------------------------------------------
-- Array utils
-----------------------------------------------------------------------------

listArrayl0         :: [a] -> Array Int a
listArrayl0 string  = listArray (0,length string - 1) string

-- (!!!) is a variant of (!), which prints out the problem in case of
-- an index out of bounds.
(!!!)   :: Array Int a -> Int -> a
a!!! n  =  if n >= fst (bounds a) && n <= snd (bounds a) 
           then a!n 
           else error (show (fst (bounds a)) ++ " " ++ show (snd (bounds a)) ++ " " ++ show n)
