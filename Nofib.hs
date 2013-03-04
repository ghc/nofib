{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}

module Main(main) where

-- Standard libraries
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Time.Clock
import qualified System.Directory as IO
import System.Exit
import System.Info
import System.IO
import System.Process

-- CmdArgs - argument parsing
import System.Console.CmdArgs

-- Shake - build system
import Development.Shake
import Development.Shake.FilePath


---------------------------------------------------------------------
-- TEST CONFIGURATION - which tests are available to run

-- | These are directories that contain tests.
testRoots :: [String]
-- testRoots = words "imaginary spectral real parallel spectral/hartel"
testRoots = words "imaginary spectral real spectral/hartel"


-- | These are tests that are under testRoots, but should be skipped (all are skipped by the Makefile system)
disabledTests :: [String]
disabledTests = words "hartel last-piece secretary triangle ebnf2ps HMMS PolyGP rx cfd dcbm linsolv warshall"


-- | These tests are compiled by the Makefile system, but don't work for me (mostly GHC 7.4 breaks)
newlyDisabledTests :: [String]
newlyDisabledTests = words "power lift fulsom fluid"


-- | Directories containing tests that the system can run.
allTests :: IO [FilePath]
allTests = do
    xs <- forM testRoots $ \x -> do
        ys <- IO.getDirectoryContents x
        return [x </> y | y <- ys, '.' `notElem` y, y `notElem` disabledTests, y `notElem` newlyDisabledTests]
    fmap sort $ flip filterM (concat xs) $ \x -> do
        b <- IO.doesDirectoryExist x
        if not b then return False else
            IO.doesFileExist $ x </> "Makefile"


---------------------------------------------------------------------
-- ARGUMENT PARSING - mostly based on CmdArgs

data Nofib
    = Clean
    | Build
        {clean :: Bool
        ,tests :: [String]
        ,way :: [String]
        ,threads :: Int
        ,compiler :: String
        ,tag :: String
        ,output :: String
        ,run :: Maybe Speed
        ,rts :: [String]
        ,times :: Int
        ,skip_check :: Bool
        }
    deriving (Data,Typeable,Show)

data Speed = Fast | Norm | Slow
    deriving (Data,Typeable,Show)


nofibMode :: Mode (CmdArgs Nofib)
nofibMode = cmdArgsMode $ modes
    [Clean
        &= help "Clean the build"
    ,Build
        {clean = False &= groupname "Building" &= help "Clean before building"
        ,tests = [] &= args &= typ "TEST"
        ,way = [] &= help "Which way to build, defaults to -O1"
        ,threads = 1 &= name "j" &= typ "NUM" &= help "Number of threads, defaults to 1"
        ,compiler = "ghc" &= help "Compiler to use, defaults to ghc"
        ,tag = "" &= help "Tag to name the compiler, defaults to compiler --version"
        ,output = "" &= help "Where to put created files under _make, defaults to tag/way"
        ,run = Nothing &= groupname "Running" &= opt "norm" &= help "Run the results (Fast,Norm,Slow)"
        ,times = 1 &= help "Number of times to run each test"
        ,rts = [] &= help "Which RTS options to pass when running"
        ,skip_check = False &= help "Skip checking the results of the tests"
        } &= auto &= help "Build"
        &= help "Build and run"
    ]
    &= summary "Nofib benchmark suite"


-- | Create a clean set of arguments, with any defaults filled in
nofibArgs :: IO Nofib
nofibArgs = do
    args <- cmdArgsRun nofibMode
    case args of
        Clean -> return args
        Build{..} -> do
            way <- return $ let xs = concatMap words way in if null xs then ["-O1"] else xs
            tag <- if tag == "" then compilerTag compiler else return tag
            tests <- resolveTests tests
            output <- return $ "_make" </> (if null output then tag </> intercalate "_" way else output)
            return Build{..}


-- | Given the tests the user asked for, expand them out, e.g. real is the full real suite.
resolveTests :: [String] -> IO [String]
resolveTests [] = allTests
resolveTests finds = do
    let slash1 x = "/" ++ map (\i -> if i == '\\' then '/' else i) x
        slash2 x = slash1 x ++ "/"
    tests <- allTests
    let whole = filter (\test -> any (\find -> slash2 find `isInfixOf` slash2 test) finds) tests -- whole matches
    let prefix = filter (\test -> any (\find -> slash1 find `isInfixOf` slash2 test) finds) tests -- prefix matches
    let res = if null whole then prefix else whole
    when (null res) $
        error $ "The targets failed to match any programs: " ++ unwords finds
    return res


-- | Find the default compiler string, e.g. ghc-7.4.1
compilerTag :: String -> IO String
compilerTag compiler = do
    (_,stdout,_) <- readProcessWithExitCode compiler ["--version"] ""
    let ver = takeWhile (\x -> isDigit x || x == '.') $ dropWhile (not . isDigit) stdout
    return $ if null ver then "unknown" else ver


---------------------------------------------------------------------
-- MAIN DRIVER

-- | Main program, just interpret the arguments and dispatch the tasks.
main = do
    args <- nofibArgs
    case args of
        Clean -> removeDirectoryRecursive "_make"
        Build{..} -> do
            when clean $
                removeDirectoryRecursive output

            linker <- newResource "ghc linker" 1
            shake shakeOptions
                {shakeThreads=threads
                ,shakeFiles=output ++ "/"
                ,shakeReport=Just $ output ++ "/shake_report.html"
                ,shakeVerbosity=Development.Shake.Loud} $
                    buildRules linker args
            putStrLn "Build completed"

            when (isJust run) $ do
                ns <- mapM (runTest args) tests
                let tot = length ns
                    bad = length $ filter not ns
                    t i = if i == 1 then "1 test" else show i ++ " tests"
                if bad == 0 then
                    putStrLn $ "Ran " ++ t tot ++ " successfully"
                 else
                    putStrLn $ "WARNING: " ++ t bad ++ " failed, out of " ++ t tot


-- | Rules to build the given tests. For each test, there are three files
--   we care about:
--
-- * config.txt - a cleaned up version of the configuration out of Makefile,
--   created by convertConfig. Also contains "MAIN" which points at the name
--   of the Main module.
-- 
-- * Main.exe - the actual binary, produced by ghc linking everything.
--
-- * Main.deps - the files that Main.exe depends on, ghc -M.
--
-- * .hi/.o - files produced by ghc -c.
--
--   Most complication comes from modules not named Main, which still produce
--   Main.o object files (I think ghc -M gets these wrong).
buildRules :: Resource -> Nofib -> Rules ()
buildRules r Build{..} = do
    let unoutput x =
            let f x = if hasExtension x then f $ takeDirectory x else x
            in f $ takeDirectory $ drop (length output + 1) x
    want $ concat
        [ [s </> "Main" <.> exe, s </> "config.txt"] | t <- tests, let s = output </> t]

    "//config.txt" *> \out -> do
        let dir = unoutput out
        src <- readFileLines $ dir </> "Makefile"
        let poss = ["Main.hs","Main.lhs",takeFileName dir <.> "hs",takeFileName dir <.> "lhs"]
        bs <- filterM (doesFileExist . (dir </>)) poss
        let mainMod = case bs of
                [] -> error $ "Could not find Main file for " ++ dir
                x:_ -> "MAIN = " ++ x
        writeFileLines out $ mainMod : convertConfig src

    ("//Main" <.> exe) *> \out -> do
        deps <- readFile' $ replaceExtension out "deps"
        let os = nub [ if isLower $ head $ takeFileName x then replaceExtension out "o" else output </> x
                     | x <- words deps, takeExtension x == ".o"]
        need os
        config <- readConfig' $ takeDirectory out </> "config.txt"
        let dir = unoutput out
            obj = takeDirectory out
            name = takeFileName dir
        putNormal $ "==nofib== " ++ name ++ " : time to link " ++ name ++ " follows..."
        withResource r 1 $
            system' compiler $ ["-Rghc-timing","-rtsopts","-o",out] ++ os ++ way ++ words (config "SRC_HC_OPTS")
        putNormal $ "==nofib== " ++ name ++ ": size of " ++ name ++ " follows..."
        system' "size" [out]

    ["//*.o","//*.hi"] *>> \[o,hi] -> do
        let dir = unoutput o
            obj = output </> dir
        config <- readConfig' $ obj </> "config.txt"
        let mod = let x = dropExtension $ drop (length obj + 1) o
                  in if x == "Main" then dropExtension $ config "MAIN" else x
        src <- do b <- doesFileExist $ dir </> mod <.> "hs"
                  return $ dir </> mod <.> (if b then "hs" else "lhs")
        deps <- readFileLines $ obj </> "Main.deps"
        need [ if takeExtension r `elem` [".h",".hs",".lhs"] then r else output </> r
             | lhs:":":rhs <- map words $ deps, dir </> mod <.> "o" == lhs, r <- rhs]
        let name = takeFileName dir
        putNormal $ "==nofib== " ++ name ++ " : time to compile " ++ mod ++ " follows..."
        system' compiler $ ["-Rghc-timing","-c",src,"-w","-i"++obj,"-odir="++obj,"-hidir="++obj] ++
                           way ++ words (config "SRC_HC_OPTS")
        putNormal $ "==nofib== " ++ name ++ ": size of " ++ takeFileName o ++ " follows..."
        system' "size" [o]

    "//Main.deps" *> \out -> do
        let dir = unoutput out
        config <- readConfig' $ takeDirectory out </> "config.txt"
        system' compiler $ ["-w","-M",dir </> config "MAIN","-i" ++ dir,"-dep-makefile=" ++ out, "-dep-suffix", ""] ++
                           words (config "SRC_HC_OPTS")
        src <- liftIO $ readFile out
        need [x | x <- words src, takeExtension x `elem` [".hs",".lhs",".h"]]


-- | Run a test, checking stdout/stderr are as expected, and reporting time.
--   Return True if the test passes.
runTest :: Nofib -> String -> IO Bool
runTest Build{run=Just speed,..} test = do
    putStrLn $ "==nofib== " ++ takeDirectory1 test ++ ": time to run " ++ takeDirectory1 test ++ " follows..."
    config <- readConfig $ output </> test </> "config.txt"
    let args = words (config "PROG_ARGS") ++ words (config $ map toUpper (show speed) ++ "_OPTS")
    stdin <- let s = config "STDIN_FILE" in if s == "" then grab "stdin" else readFile $ test </> s
    stats <- IO.canonicalizePath $ output </> test </> "stat.txt"

    fmap and $ replicateM times $ do
        start <- getCurrentTime
        (code,stdout,stderr) <- readProcessWithExitCodeAndWorkingDirectory
            test (output </> test </> "Main" <.> exe) (args++"+RTS":rts++["-t"++stats]) stdin
        end <- getCurrentTime
        stdoutWant <- grab "stdout"
        stderrWant <- grab "stderr"
        writeFile (output </> test </> "stdout") stdout
        writeFile (output </> test </> "stderr") stderr
        putStrLn $ show (floor $ fromRational (toRational $ end `diffUTCTime` start) * 1000) ++ "ms"
        putStr =<< readFile stats
        err <- return $
            if not skip_check && stderr /= stderrWant then "FAILED STDERR\nWANTED: " ++ snip stderrWant ++ "\nGOT: " ++ snip stderr
            else if not skip_check && stdout /= stdoutWant then "FAILED STDOUT\nWANTED: " ++ snip stdoutWant ++ "\nGOT: " ++ snip stdout
            else if not skip_check && code /= ExitSuccess then "FAILED EXIT CODE " ++ show code
            else ""
        if null err then return True else putStrLn err >> return False
    where
        snip x = if length x > 200 then take 200 x ++ "..." else x

        grab ext = do
            let s = [test </> takeFileName test <.> map toLower (show speed) ++ ext
                    ,test </> takeFileName test <.> ext]
            ss <- filterM IO.doesFileExist s
            maybe (return "") readFile $ listToMaybe ss


---------------------------------------------------------------------
-- CONFIGURATION UTILITIES
-- The Makefile's are slurped for configuration, to produce a cleaned-up config file

-- | Given the source of a Makefile, slurp out the configuration strings.
convertConfig :: [String] -> [String]
convertConfig xs = [remap a ++ " = " ++ b | x <- xs, let (a,b) = separate x, a `elem` keep]
    where
        keep = words "PROG_ARGS SRC_HC_OPTS SRC_RUNTEST_OPTS SLOW_OPTS NORM_OPTS FAST_OPTS STDIN_FILE"
        remap "SRC_RUNTEST_OPTS" = "PROG_ARGS"
        remap x = x

        separate x = (name,rest)
            where (name,x2) = span (\x -> isAlpha x || x == '_') x
                  rest = dropWhile isSpace $ dropWhile (`elem` "+=") $ dropWhile isSpace x2


-- | Read a configuration file (new format) into a function supplying options.
readConfig :: FilePath -> IO (String -> String)
readConfig x = do
    src <- readFile x
    let res = [ (reverse $ dropWhile isSpace $ reverse a, dropWhile isSpace $ drop 1 b)
              | y <- lines src, let (a,b) = break (== '=') y]
    return $ \x -> fromMaybe "" $ lookup x res


-- | readConfig lifted into the Action monad.
readConfig' :: FilePath -> Action (String -> String)
readConfig' x = do
    need [x]
    liftIO $ readConfig x


---------------------------------------------------------------------
-- GENERAL UTILITIES

-- | The executable extension on this platform.
exe :: String
exe = if os == "mingw32" then "exe" else ""


-- | Like the standard removeDirectoryRecursive, but doesn't fail if the path is missing.
removeDirectoryRecursive :: FilePath -> IO ()
removeDirectoryRecursive x = do
    b <- IO.doesDirectoryExist x
    when b $ IO.removeDirectoryRecursive x


-- | Source for readProcessWithExitCode, plus addition of cwd
readProcessWithExitCodeAndWorkingDirectory
    :: FilePath                 -- ^ directory to use
    -> FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> String                   -- ^ standard input
    -> IO (ExitCode,String,String) -- ^ exitcode, stdout, stderr
readProcessWithExitCodeAndWorkingDirectory cwd cmd args input = do
    (Just inh, Just outh, Just errh, pid) <-
        createProcess (proc cmd args){ cwd     = Just cwd,
                                       std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = CreatePipe }
    outMVar <- newEmptyMVar
    out  <- hGetContents outh
    _ <- forkIO $ evaluate (length out) >> putMVar outMVar ()
    err  <- hGetContents errh
    _ <- forkIO $ evaluate (length err) >> putMVar outMVar ()
    when (not (null input)) $ do hPutStr inh input; hFlush inh
    hClose inh
    takeMVar outMVar
    takeMVar outMVar
    hClose outh
    hClose errh
    ex <- waitForProcess pid

    return (ex, out, err)
