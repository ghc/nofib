
\author{John Meacham john@repetae.net}
\date{Mon, 10 Sep 2001 02:14:24 -0700}

\begin{document}

\section{colored ASCII output?}

For simple quick-and-dirty stuff see my attatched ANSI.hs file which
encodes a few basic ansi sequences including color changing ones
 and cursor control. If you want to do it the right (and
more complicated) way however I suggest you find an haskell binding for
the 'curses' library, one comes with QForeign in the examples directory
based on hsc2hs, it should be portable to what comes with ghc 5 easilly
now that QForeign and ghc's functionality are merging... (BTW, can the
excellent QForeign examples, Curses, Db, Bzip, zlib and libgr be
included in hslibs?)

below is a simple module which exports some ANSI control codes which
should work on any terminal (DOS ANSI, vt102, xterm, rxvt, linux
console, etc...) to use just concat the strings with whatever you wish
to output, attrFG and attrBG control the foreground and background
colors respectivly, what numbers map to which colors is an excersize
left to the reader.
        John

\begin{code}

module ANSI(
    move,
    clrToEOL,
    clrFromBOL,
    clrLine,
    clrToEOS,
    clrFrombos,
    clrScreen,
    attrClear,
    attrBold,
    attrUnderline,
    attrBlink,
    attrReverse,
    cursorOn,
    cursorOff,
    attrFG,
    attrBG,
) where

import Prelude((++), show, Int, String)

move ::  Int -> Int -> String
move x y = "\27[" ++ (show y) ++ ";" ++ (show x) ++ ";H"

clrToEOL, clrFromBOL, clrLine, clrToEOS, clrFrombos, clrScreen, attrClear,
 attrBold, attrUnderline, attrBlink, attrReverse, cursorOn, cursorOff :: String
clrToEOL = "\27[K"
clrFromBOL = "\27[1K"
clrLine = "\27[2K"
clrToEOS = "\27[J"
clrFrombos = "\27[1J"
clrScreen = "\27[2J"
attrClear = "\27[0m"
attrBold = "\27[1m"
attrUnderline = "\27[4m"
attrBlink = "\27[5m"
attrReverse = "\27[7m"
cursorOn = "\27[?25h"
cursorOff = "\27[?25l"

attrFG :: Int -> String
attrFG c =  "\27[3" ++ (show c) ++ "m"
attrBG :: Int -> String
attrBG c =  "\27[4" ++ (show c) ++ "m"

\end{code}
\end{document}