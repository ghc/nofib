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

module IOSupplement (PathCont, getPath, readPathFile)
where

import System -- 1.3
import IOBase ( IOError (..) )
--------------------------------------------------------------------------------

type PathCont = [String] -> IO ()
type FailCont = IOError -> IO ()
type StrCont  = String -> IO ()

getPath :: String -> [String] -> PathCont -> IO ()
--
-- accepts the name of an environment variable and a [String] of default paths
-- and calls the continuation (::PathCont) with the resulting search path
--
getPath envVar dflt cont =
  (do {path <- getEnv envVar; cont (manglePath path dflt)})
    `catch` 
       (\ (NoSuchThing _) -> cont dflt)
   

-- mangle a colon separated pathstring with a default path

manglePath :: String -> [String] -> [String]
manglePath "" dflt = dflt
manglePath cs dflt = case span (/= ':') cs of
	                       ("",':':cs') -> dflt ++ manglePath cs' []
			       ("", "") -> dflt
			       (path,':':cs') -> path: manglePath cs' dflt
			       (path,"") -> [path]

--------------------------------------------------------------------------------

readPathFile :: [String] -> String -> FailCont -> StrCont -> IO ()
--
-- readPathFile searchPath fileName fc sc
-- scan searchPath for fileName and read it
-- unless fileName starts with '.' or is absolute (starts with '/')
--
readPathFile _  fileName@('/':_) fc sc = myreadFile fileName fc sc
readPathFile _  fileName@('.':_) fc sc = myreadFile fileName fc sc
readPathFile [] fileName fc sc =
	fc (userError ("readPathFile failed on :" ++ fileName))
readPathFile (path: paths) fileName fc sc =
--	appendChan stderr ("Trying path "++fullName++"...\n") exit
	(myreadFile fullName failCont sc)
    where
	fullName   = path ++ '/': fileName
	failCont _ = readPathFile paths fileName fc sc


myreadFile :: String -> FailCont -> StrCont -> IO ()
myreadFile filename fc sc
  = catch (readFile filename	>>= \ cts -> sc cts)
	  fc
