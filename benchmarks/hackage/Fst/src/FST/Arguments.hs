{-
   **************************************************************
   * Filename      : Arguments.hs                               *
   * Author        : Markus Forsberg                            *
   *                 d97forma@dtek.chalmers.se                  *
   * Last Modified : 6 July, 2001                               *
   * Lines         : -                                          *
   **************************************************************
-}

module FST.Arguments ( parseInteractive,
                   InteractiveCommand(..),
                   isFST,
                   isDAT,
                   isNET,
                   isTHIS,
                   parseBatch,
                   inputB,
                   outputB,
                   isUpB
                 ) where

import FST.GetOpt

data InteractiveCommand = BuildTransducer                |
                          BuildNTransducer               |
                          Minimize                       |
                          Determinize                    |
			  StdInReg String                |
			  Load FilePath                  |
                          LUnion FilePath FilePath       |
                          LProduct FilePath FilePath     |
                          LStar FilePath                 |
                          LComposition FilePath FilePath |
			  Save FilePath                  |
			  ApplyDown                      |
			  ApplyUp                        |
			  ApplyD [String]                |
			  ApplyU [String]                |
			  ViewReg                        |
			  ViewInput                      |
			  ViewOutput                     |
                          ViewTransducer                 |
			  Help                           |
			  ClearMemory                    |
			  Quit                           |
			  NoCommand

parseInteractive :: [String] -> InteractiveCommand
parseInteractive ["b"]                   = BuildTransducer
parseInteractive ["bn"]                  = BuildNTransducer
parseInteractive ["m"]                   = Minimize
parseInteractive ["det"]                 = Determinize
parseInteractive ("r":xs)                = StdInReg (unwords xs)
parseInteractive ["d"]                   = ApplyDown
parseInteractive ["u"]                   = ApplyUp
parseInteractive ("d":xs)                = ApplyD xs
parseInteractive ("u":xs)                = ApplyU xs
parseInteractive ["l",file]              = Load file
parseInteractive ["l",file1,"|",file2]   = LUnion file1 file2
parseInteractive ["l",file1," ",file2]   = LProduct file1 file2
parseInteractive ["l",file, "*"]         = LStar file
parseInteractive ["l",file1,".o.",file2] = LComposition file1 file2
parseInteractive ["s",file]              = Save file
parseInteractive ["vt"]                  = ViewTransducer
parseInteractive ["vi"]                  = ViewInput
parseInteractive ["vo"]                  = ViewOutput
parseInteractive ["vr"]                  = ViewReg
parseInteractive ["h"]                   = Help
parseInteractive ["q"]                   = Quit
parseInteractive ["c"]                   = ClearMemory
parseInteractive _                       = NoCommand

isFST :: String -> Bool
isFST str = case (reverse str) of
	     ('t':'s':'f':'.':_)  -> True
	     _                    -> False

isDAT :: String -> Bool
isDAT str = case (reverse str) of
	     ('t':'a':'d':'.':_)  -> True
	     _                    -> False

isNET :: String -> Bool
isNET str = case (reverse str) of
             ('t':'e':'n':'.':_) -> True
             _                 -> False

isTHIS :: String -> Bool
isTHIS = (== "*")

isApplyUp :: [String] -> Bool
isApplyUp = elem "-u"

data BatchCommand = DownB                   |
		    UpB                     |
		    InvalidCommand          |
		    Input String            |
		    Output String           |
		    HelpB
 deriving Show

batchOptions :: [OptDescr BatchCommand]
batchOptions = [Option ['u'] ["up"]     (NoArg UpB)             "apply the transducer up (default is down)",
                Option ['d'] ["down"]   (NoArg DownB)           "apply the transducer down (default)",
                Option ['i'] ["input"]  (ReqArg Input "FILE")  "read input from FILE",
                Option ['o'] ["output"] (ReqArg Output "FILE") "write output to FILE"]

parseBatch :: [String] -> Either String (FilePath,[BatchCommand])
parseBatch cmdline = case getOpt Permute batchOptions cmdline of
                      (o,[file],[]) -> Right (file,o)
                      (_,_,errs)    -> Left $ concat errs ++ usageInfo header batchOptions
 where header = "Usage: fst [FILE.net or FILE.fst] [OPTIONS...]"

inputB :: [BatchCommand] -> Maybe FilePath
inputB               [] = Nothing
inputB ((Input file):_) = return file
inputB (_:xs)           = inputB xs

outputB :: [BatchCommand] -> Maybe FilePath
outputB []                = Nothing
outputB ((Output file):_) = return file
outputB (_:xs)            = outputB xs

isUpB :: [BatchCommand] -> Bool
isUpB []      = False
isUpB (UpB:_) = True
isUpB (_:xs)  = isUpB xs

{-
-----------------------------------------------------------------------------------------
-- and here a small and hopefully enlightening example:

data Flag = Verbose | Version | Name String | Output String | Arg String   deriving Show

options :: [OptDescr Flag]
options =
   [Option ['v']     ["verbose"]           (NoArg Verbose)      "verbosely list files",
    Option ['V','?'] ["version","release"] (NoArg Version)      "show version info",
    Option ['o']     ["output"]            (OptArg out "FILE")  "use FILE for dump",
    Option ['n']     ["name"]              (ReqArg Name "USER") "only dump USER's files"]

out :: Maybe String -> Flag
out Nothing  = Output "stdout"
out (Just o) = Output o

test :: ArgOrder Flag -> [String] -> String
test order cmdline = case getOpt order options cmdline of
                        (o,n,[]  ) -> "options=" ++ show o ++ "  args=" ++ show n ++ "\n"
                        (_,_,errs) -> concat errs ++ usageInfo header options
   where header = "Usage: foobar [OPTION...] files..."

-- example runs:
-- putStr (test RequireOrder ["foo","-v"])
--    ==> options=[]  args=["foo", "-v"]
-- putStr (test Permute ["foo","-v"])
--    ==> options=[Verbose]  args=["foo"]
-- putStr (test (ReturnInOrder Arg) ["foo","-v"])
--    ==> options=[Arg "foo", Verbose]  args=[]
-- putStr (test Permute ["foo","--","-v"])
--    ==> options=[]  args=["foo", "-v"]
-- putStr (test Permute ["-?o","--name","bar","--na=baz"])
--    ==> options=[Version, Output "stdout", Name "bar", Name "baz"]  args=[]
-- putStr (test Permute ["--ver","foo"])
--    ==> option `--ver' is ambiguous; could be one of:
--          -v      --verbose             verbosely list files
--          -V, -?  --version, --release  show version info
--        Usage: foobar [OPTION...] files...
--          -v        --verbose             verbosely list files
--          -V, -?    --version, --release  show version info
--          -o[FILE]  --output[=FILE]       use FILE for dump
--          -n USER   --name=USER           only dump USER's files
-----------------------------------------------------------------------------------------

test :: ArgOrder BatchCommand -> [String] -> String
test order cmdline = case getOpt order batchOptions cmdline of
                        (o,n,[]  ) -> "options=" ++ show o ++ "  args=" ++ show n ++ "\n"
                        (_,_,errs) -> concat errs ++ usageInfo header batchOptions
   where header = "Usage: fst [OPTION...] files..."
-}
