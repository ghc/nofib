module CommandLine (parse_cmds) where
-- Copyright 1994 by Peter Thiemann

defaultArgs :: Args
defaultArgs  =  MkArgs "Times-Roman" 10 "black" "Times-Roman" 10 "black" "black" "black" 500 500 30 100 200 "rgb.txt" False False True False False False

usage :: Dialogue
usage  =  appendChan stderr "Usage: prog [-ntFont String] [-ntScale Int] [-ntColor String] [-tFont String] [-tScale Int] [-tColor String] [-lineColor String] [-fatLineColor String] [-borderDistX Int] [-borderDistY Int] [-lineWidth Int] [-fatLineWidth Int] [-arrowSize Int] [-rgbFileName String] [-happy] [(+|-)simplify] [(+|-)ps] [(+|-)fig] [-help] [-verbose]" exit done

data Args  =  MkArgs String Int String String Int String String String Int Int Int Int Int String Bool Bool Bool Bool Bool Bool deriving ()
type ProgType = String -> Int -> String -> String -> Int -> String -> String -> String -> Int -> Int -> Int -> Int -> Int -> String -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> [String] -> Dialogue

parse_args :: ProgType -> Args -> [String] -> Dialogue
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20) ("-ntFont":rest)
    =  readstring (\str -> parse_args prog (MkArgs str x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20) ("-ntScale":rest)
    =  readval reads (\val -> parse_args prog (MkArgs x1 val x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20) ("-ntColor":rest)
    =  readstring (\str -> parse_args prog (MkArgs x1 x2 str x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20) ("-tFont":rest)
    =  readstring (\str -> parse_args prog (MkArgs x1 x2 x3 str x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20) ("-tScale":rest)
    =  readval reads (\val -> parse_args prog (MkArgs x1 x2 x3 x4 val x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20) ("-tColor":rest)
    =  readstring (\str -> parse_args prog (MkArgs x1 x2 x3 x4 x5 str x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20) ("-lineColor":rest)
    =  readstring (\str -> parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 str x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20) ("-fatLineColor":rest)
    =  readstring (\str -> parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 str x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20) ("-borderDistX":rest)
    =  readval reads (\val -> parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 val x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20) ("-borderDistY":rest)
    =  readval reads (\val -> parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 val x11 x12 x13 x14 x15 x16 x17 x18 x19 x20)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20) ("-lineWidth":rest)
    =  readval reads (\val -> parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 val x12 x13 x14 x15 x16 x17 x18 x19 x20)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20) ("-fatLineWidth":rest)
    =  readval reads (\val -> parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 val x13 x14 x15 x16 x17 x18 x19 x20)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20) ("-arrowSize":rest)
    =  readval reads (\val -> parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 val x14 x15 x16 x17 x18 x19 x20)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20) ("-rgbFileName":rest)
    =  readstring (\str -> parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 str x15 x16 x17 x18 x19 x20)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20) ("-happy":rest)
    =  readbool (parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 True x16 x17 x18 x19 x20)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20) ("-simplify":rest)
    =  readbool (parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 False x17 x18 x19 x20)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20) ("+simplify":rest)
    =  readbool (parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 True x17 x18 x19 x20)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20) ("-ps":rest)
    =  readbool (parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 False x18 x19 x20)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20) ("+ps":rest)
    =  readbool (parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 True x18 x19 x20)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20) ("-fig":rest)
    =  readbool (parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 False x19 x20)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20) ("+fig":rest)
    =  readbool (parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 True x19 x20)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20) ("-help":rest)
    =  readbool (parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 True x20)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20) ("-verbose":rest)
    =  readbool (parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 True)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20) (('-': _) :rest)
    =  usage
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20)  rest  =  prog x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 rest

parse_cmds :: ProgType -> Dialogue
parse_cmds prog =  getArgs exit (parse_args prog defaultArgs)

readbool :: ([String] -> Dialogue) -> [String] -> Dialogue
readbool f = f

readstring :: (String -> [String] -> Dialogue) -> [String] -> Dialogue
readstring f (str: rest) = f str rest
readstring f []          = usage

readval :: (Text a) => ReadS a -> (a -> [String] -> Dialogue) -> [String]
                       -> Dialogue
readval readsfn f (str: rest)
    =  case readsfn str of
           ((val, ""):_) -> f val rest
           _             -> usage
