-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Algorithms.Palindromes.Options
-- Copyright   :  (c) 2007 - 2010 Johan Jeuring
-- License     :  BSD3
--
-- Maintainer  :  johan@jeuring.net
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Data.Algorithms.Palindromes.Options where

import System.Console.GetOpt 

import Data.Algorithms.Palindromes.Palindromes

-----------------------------------------------------------------------------
-- flags
-----------------------------------------------------------------------------

data Flag  =  Help
           |  StandardInput
           |  LongestPalindrome
           |  LongestPalindromes
           |  LengthLongestPalindrome
           |  LengthLongestPalindromes
           |  LongestTextPalindrome
           |  LongestTextPalindromes
           |  LongestWordPalindrome
           |  LongestWordPalindromes
           |  LengthAtLeast Int deriving Show
          

isHelp :: Flag -> Bool
isHelp Help  =  True
isHelp _     =  False      

isStandardInput :: Flag -> Bool
isStandardInput StandardInput  =  True
isStandardInput _              =  False      

isLengthAtLeast :: Flag -> Bool
isLengthAtLeast (LengthAtLeast _)  =  True
isLengthAtLeast _                  =  False      

getLength :: Flag -> Int
getLength (LengthAtLeast n)  =  n 
getLength _                  =  error "No length specified"

-- I am using single letter options here (except for help): getOpt handles 
-- options too flexible: in case a letter within a multiple letter option is 
-- recognized, it is taken as a single letter option.
options :: [OptDescr Flag] 
options = 
  [Option "h" ["help"] (NoArg Help)
     "This message"
  ,Option "i" [] (NoArg StandardInput)
     "Read input from standard input"
  ,Option "o" [] (NoArg LongestPalindrome) 
     "Longest palindrome (default)"
  ,Option "p" [] (NoArg LongestPalindromes)
     "Longest palindrome around each position in the input"
  ,Option "k" [] (NoArg LengthLongestPalindrome)
     "Length of the longest palindrome"
  ,Option "l" [] (NoArg LengthLongestPalindromes)
     "Length of the longest palindrome around each position in the input"
  ,Option "s" [] (NoArg LongestTextPalindrome)
     "Longest palindrome ignoring case, spacing and punctuation"
  ,Option "t" [] (NoArg LongestTextPalindromes)
     "Longest text palindrome around each position in the input"
  ,Option "v" [] (NoArg LongestWordPalindrome)
     "Longest text palindrome preceded and followed by punctuation symbols (if any)"
  ,Option "w" [] (NoArg LongestWordPalindromes)
     "Longest word palindrome around each position in the input"
  ,Option "m" [] (ReqArg (LengthAtLeast . (read :: String -> Int)) "arg")
     "Palindromes of length at least [arg]"
  ]

handleOptions :: [Flag] -> (String -> String,Bool)
handleOptions flags = 
  let lal     =  filter isLengthAtLeast flags
      flags'  =  filter (\f -> not (isLengthAtLeast f || isStandardInput f)) flags
      m       =  if null lal 
                 then 0
                 else getLength (head lal) 
      fromfile = null $ filter isStandardInput flags 
      function = case flags' of
                   []        -> longestPalindrome
                   (flag:_)  -> 
                     case flag of 
                       Help                      ->  
                         const (usageInfo headerHelpMessage options)
                       LongestPalindrome         ->  longestPalindrome 
                       LongestPalindromes        ->  longestPalindromes m
                       LengthLongestPalindrome   ->  lengthLongestPalindrome 
                       LengthLongestPalindromes  ->  lengthLongestPalindromes
                       LongestTextPalindrome     ->  longestTextPalindrome 
                       LongestTextPalindromes    ->  longestTextPalindromes m
                       LongestWordPalindrome     ->  longestWordPalindrome		
                       LongestWordPalindromes    ->  longestWordPalindromes m		
                       _                         ->  error "handleOptions"
  in (function,fromfile)

headerHelpMessage :: String
headerHelpMessage = 
     "*********************\n"
  ++ "* Palindrome Finder *\n"
  ++ "*********************\n"
  ++ "Usage:"
  
  
 