module Main where

import PreludeGlaST

main =
	appendChan stdout (shows res "\n")
	exit done
	where
	res = unsafePerformPrimIO (
		ca `thenPrimIO` \r -> returnPrimIO r)
	ca :: PrimIO Float
	ca = _ccall_ nn
