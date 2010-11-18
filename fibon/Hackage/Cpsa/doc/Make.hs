-- A simple, CPSA specific make system

module Make (cpsa, shapes, annos, cleanse, get, set,
             build, clean, roots) where

{- Place a copy of this source file in the directory used to store
CPSA problem statements, edit it to suit your needs, and load it into
a Haskell interpreter.

Normally, just the build and the clean command are used.  It's the
build command that you usually modify.

To analyze a problem in prob.scm, type:

*Make> cpsa "prob"

If successful, the analysis is in the file prob.xml, which can be
viewed with a standards-compliant browser.

To analyze a problem in prob.sch using the Diffie-Hellman algebra, type:

*Make> cpsa "prob"

For a shapes only version of the analysis, type:

*Make> shapes "prob"

If successful, the shapes are in the file prob_shapes.xml.

When the protocol is annotated with rely-guarantee formulas, type:

*Make> annos "prob"

If successful, the annotated shapes are in the file prob_annotations.xml.

To remove the files generated from source files, type:

*Make> cleanse "prob"

To see the command-line options used by CPSA, type:

*Make> get

To change the command-line options used by CPSA to "-b 15", type:

*Make> set "-b 15"

To analyze all source files in the directory, type:

*Make> build

To remove the files generated from source files in the directory, type:

*Make> clean

-}

import Control.Monad (mapM_)
import Data.List (sort)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System (ExitCode (..), system)
import System.IO (putStrLn)
import System.IO.Unsafe (unsafePerformIO)
import System.FilePath (FilePath, splitExtension)
import System.Directory (removeFile, doesFileExist, getModificationTime,
                         getCurrentDirectory, getDirectoryContents)

-- Flags for CPSA

initialCpsaFlags :: String
initialCpsaFlags = "+RTS -M512m -RTS"

-- A mutable location for CPSA flags
cpsaFlags :: IORef String
cpsaFlags = unsafePerformIO $ newIORef initialCpsaFlags

-- Get the CPSA flags
get :: IO String
get =
    readIORef cpsaFlags

-- Set the CPSA flags
set :: String -> IO ()
set flags =
    writeIORef cpsaFlags flags

-- Transformation rules

data Rule = Rule
    { prog :: String,           -- program to run
      inputExt :: String,       -- input file name extension
      outputExt :: String }     -- output file name extension

-- Graph Rule

graph :: FilePath -> IO ()
graph root =
    make graphRule root         -- make graph using given rule

graphRule :: Rule
graphRule =
    Rule { prog = "cpsagraph -x",
           inputExt = cpsaExt,
           outputExt = graphExt }

-- CPSA Rule

cpsa :: FilePath -> IO ()
cpsa root =
    do
      cpsaAll root
      shapes root
      graph root

cpsaAll :: FilePath -> IO ()
cpsaAll root =
    do
      exists <- doesFileExist (root ++ sourceDhExt)
      case exists of
        True -> cpsaDh root
        False -> cpsaBasic root

-- CPSA using Basic rule

cpsaBasic :: FilePath -> IO ()
cpsaBasic root =
    do
      flags <- get               -- get CPSA flags
      make (cpsaBasicRule flags) root -- make CPSA output using given rule

cpsaBasicRule :: String -> Rule
cpsaBasicRule flags =
    Rule { prog = "cpsa " ++ flags,
           inputExt = sourceBasicExt,
           outputExt = cpsaExt }

-- CPSA using Diffie-Hellman Rule

cpsaDh :: FilePath -> IO ()
cpsaDh root =
    do
      flags <- get               -- get CPSA flags
      make (cpsaDhRule flags) root -- make CPSA output using given rule

cpsaDhRule :: String -> Rule
cpsaDhRule flags =
    Rule { prog = "cpsa -a diffie-hellman " ++ flags,
           inputExt = sourceDhExt,
           outputExt = cpsaExt }

-- Shapes Rule

shapes :: FilePath -> IO ()
shapes root =
    do
      cpsaAll root              -- Run CPSA if need be
      make shapesRule root
      graph $ root ++ shapesRoot

shapesRule :: Rule
shapesRule =
    Rule { prog = "cpsashapes",
           inputExt = cpsaExt,
           outputExt = shapesRoot ++ cpsaExt }

-- Annotations Rule

annos :: FilePath -> IO ()
annos root =
    do
      cpsa root                 -- Run CPSA and make shapes
      make annosRule root
      graph $ root ++ annosRoot

annosRule :: Rule
annosRule =
    Rule { prog = "cpsaannotations",
           inputExt = shapesRoot ++ cpsaExt,
           outputExt = annosRoot ++ cpsaExt }

-- Parameters Rule

params :: FilePath -> IO ()
params root =
     do
       make cpsaparametersRule root

cpsaparametersRule :: Rule
cpsaparametersRule = 
    Rule { prog = "cpsaparameters",
    	   inputExt = sourceBasicExt,
	   outputExt = paramsExt }

-- Clean generated files

cleanse :: FilePath -> IO ()
cleanse root =
    do
      rm $ root ++ cpsaExt
      rm $ root ++ graphExt
      rm $ root ++ shapesRoot ++ cpsaExt
      rm $ root ++ shapesRoot ++ graphExt
      rm $ root ++ annosRoot ++ cpsaExt
      rm $ root ++ annosRoot ++ graphExt

-- File Extensions

sourceBasicExt :: String
sourceBasicExt = ".scm"

-- Diffie-hellman source file
sourceDhExt :: String
sourceDhExt = ".sch"

cpsaExt :: String
cpsaExt = ".txt"

shapesRoot :: String
shapesRoot = "_shapes"

annosRoot :: String
annosRoot = "_annotations"

graphExt :: String
graphExt = ".xml"

paramsExt :: String
paramsExt = ".params"

-- Rule Interpreters

-- Make output for root using rule
make :: Rule -> FilePath -> IO ()
make rule root =
    do
      let input = root ++ inputExt rule
      let output = root ++ outputExt rule
      done <- made input output
      case done of
        True -> return ()       -- Nothing to do
        False -> run (prog rule) input output

-- See if an output file is up-to-date
made :: FilePath -> FilePath -> IO Bool
made input output =
    do
      src <- doesFileExist input
      dst <- doesFileExist output
      case src && dst of
        False -> return False
        True ->
            do
              src <- getModificationTime input
              dst <- getModificationTime output
              return $ src < dst

-- Run a program with input and output from files

-- Print the command before running it.  Delete the output when the
-- command fails.
run :: String -> FilePath -> FilePath -> IO ()
run prog input output =
    do
      let cmd = prog ++ " -o " ++ output ++ " " ++ input
      putStrLn cmd
      code <- system cmd
      case code of
        ExitSuccess -> return ()
        ExitFailure _ ->
            do
              --rm output
              fail "Command failed"

-- Remove a file

-- Prints the command when there is a file to be deleted.
rm :: FilePath -> IO ()
rm output =
    do
      exists <- doesFileExist output
      case exists of
        False -> return ()      -- File doesn't exist
        True ->
            do                  -- Print command before removal
              putStrLn $ "rm " ++ output
              removeFile output

-- Return the roots of the CPSA source files in the current directory.

roots :: [String] -> IO [FilePath]
roots exts =
    do
      dir <- getCurrentDirectory
      files <- getDirectoryContents dir
      let roots = [ root |
                    file <- files,
                    let (root, ext) = splitExtension file,
                    elem ext exts ] -- Filter for source files
      return $ sort roots

-- Build the shapes for all the source files in the current directory.

build :: IO ()
build =
    do
      probs <- roots [sourceBasicExt, sourceDhExt]
      mapM_ cpsa probs

-- Clean files generated for all the source files in the current directory.

clean :: IO ()
clean =
    do
      probs <- roots [sourceBasicExt, sourceDhExt]
      mapM_ cleanse probs
