{-# OPTIONS -cpp #-}
--                            -*- Mode: Haskell -*- 
-- Copyright 1994 by Peter Thiemann
-- IOSupplement.hs --- some enhancements to the IO operations
-- Author          : Peter Thiemann
-- Created On      : Mon Aug 30 09:41:30 1993
-- Last Modified By: Peter Thiemann
-- Last Modified On: Thu Dec  2 10:37:39 1993
-- Update Count    : 13
-- Status          : Unknown, Use with caution!
-- 
-- $Log: IOSupplement.hs,v $
-- Revision 1.7  2002/01/29 11:03:21  simonmar
-- Tweaks to make the real suite run with GHCi.
--
-- Revision 1.6  1999/01/18 19:38:46  sof
-- Misc (backward compatible) changes to make srcs acceptable
-- to a Haskell 98 compiler.
--
-- Revision 1.5  1998/02/19 17:02:22  simonm
-- updates for library re-organisation in GHC 3.01.
--
-- Revision 1.4  1997/03/17 20:35:25  simonpj
-- More small changes towards 2.02
--
-- Revision 1.3  1997/03/14 08:08:09  simonpj
-- Major update to more-or-less 2.02
--
-- Revision 1.2  1996/07/25 21:23:58  partain
-- Bulk of final changes for 2.01
--
-- Revision 1.1  1996/01/08 20:02:33  partain
-- Initial revision
--
-- Revision 1.2  1994/03/15  15:34:53  thiemann
-- generalized readPathFile
--
-- Revision 1.1  1993/08/31  12:31:32  thiemann
-- Initial revision
--
-- $Locker:  $
--

module IOSupplement (
	getPath, readPathFile
   ) where

import System -- 1.3
import IO

#if __HASKELL1__ >= 5
#define fail ioError
#endif

--------------------------------------------------------------------------------


getPath :: String -> [String] ->  IO [String]

-- Accepts the name of an environment variable and a [String] of default paths
-- and calls the continuation (::PathCont) with the resulting search path

getPath envVar dflt =
  (do {path <- getEnv envVar; return (manglePath path dflt)})
    `catch` 
       (\ _ -> return dflt)
   

-- mangle a colon separated pathstring with a default path

manglePath :: String -> [String] -> [String]
manglePath "" dflt = dflt
manglePath cs dflt = case span (/= ':') cs of
	                       ("",':':cs') -> dflt ++ manglePath cs' []
			       ("", "") -> dflt
			       (path,':':cs') -> path: manglePath cs' dflt
			       (path,"") -> [path]

--------------------------------------------------------------------------------

readPathFile :: [String] -> String -> IO String

-- readPathFile searchPath fileName fc sc
-- scan searchPath for fileName and read it
-- unless fileName starts with '.' or is absolute (starts with '/')

readPathFile _  fileName@('/':_) = readFile fileName
readPathFile _  fileName@('.':_) = readFile fileName

readPathFile [] fileName 
  = fail (userError ("readPathFile failed on :" ++ fileName))

readPathFile (path: paths) fileName
  = readFile fullName `catch` 
    (\ _ -> readPathFile paths fileName)
  where
	fullName   = path ++ '/': fileName

