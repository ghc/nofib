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

module IOSupplement (PathCont (..), getPath, readPathFile)
where

--------------------------------------------------------------------------------

type PathCont = [String] -> Dialogue

getPath :: String -> [String] -> PathCont -> Dialogue
--
-- accepts the name of an environment variable and a [String] of default paths
-- and calls the continuation (::PathCont) with the resulting search path
--
getPath envVar dflt cont =
	getEnv envVar	(\_ -> cont dflt)
			(\path -> cont (manglePath path dflt))

-- mangle a colon separated pathstring with a default path

manglePath :: String -> [String] -> [String]
manglePath "" dflt = dflt
manglePath cs dflt = case span (/= ':') cs of
	                       ("",':':cs') -> dflt ++ manglePath cs' []
			       ("", "") -> dflt
			       (path,':':cs') -> path: manglePath cs' dflt
			       (path,"") -> [path]

--------------------------------------------------------------------------------

readPathFile :: [String] -> String -> FailCont -> StrCont -> Dialogue
--
-- readPathFile searchPath fileName fc sc
-- scan searchPath for fileName and read it
-- unless fileName starts with '.' or is absolute (starts with '/')
--
readPathFile _  fileName@('/':_) fc sc = readFile fileName fc sc
readPathFile _  fileName@('.':_) fc sc = readFile fileName fc sc
readPathFile [] fileName fc sc =
	fc (SearchError ("readPathFile failed on :" ++ fileName))
readPathFile (path: paths) fileName fc sc =
--	appendChan stderr ("Trying path "++fullName++"...\n") exit
	(readFile fullName failCont sc)
    where
	fullName   = path ++ '/': fileName
	failCont _ = readPathFile paths fileName fc sc

