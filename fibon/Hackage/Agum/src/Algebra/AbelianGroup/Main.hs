-- A top-level loop for unification and matching in Abelian groups
--
-- Copyright (C) 2009 John D. Ramsdell
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Main (main, test) where

import System.IO (isEOF, hFlush, stdout)
import Algebra.AbelianGroup.UnificationMatching

-- Test Routine

-- Given an equation, display a unifier and a matcher.
test :: String -> IO ()
test prob =
    case readM prob of
      Err err -> putStrLn err
      Ans (Equation (t0, t1)) ->
          do
            putStr "Problem:   "
            print $ Equation (t0, t1)
            let subst = unify $ Equation (t0, t1)
            putStr "Unifier:   "
            print subst
            putStr "Matcher:   "
            case match $ Equation (t0, t1) of
              Err err -> putStrLn err
              Ans subst -> print subst
            putStrLn ""

readM :: (Read a, Monad m) => String -> m a
readM s =
    case [ x | (x, t) <- reads s, ("", "") <- lex t ] of
      [x] -> return x
      [] -> fail "no parse"
      _ -> fail "ambiguous parse"

data AnsErr a
    = Ans a
    | Err String

instance Monad AnsErr where
    (Ans x) >>= k = k x
    (Err s) >>= _ = Err s
    return        = Ans
    fail          = Err

-- Main loop

main :: IO ()
main =
    do
      putStrLn "Abelian group unification and matching -- :? for help"
      loop

loop :: IO ()
loop =
    do
      putStr "agum> "
      hFlush stdout
      done <- isEOF
      case done of
        True ->
            do
              putStrLn ""
              return ()
        False ->
            do
              line <- getLine
              case () of
                _ | line == ":?" || line == ":help" ->
                      do
                        help
                        loop
                  | line == ":quit" ->
                      return ()
                  | otherwise ->
                      do
                        test line
                        loop

help :: IO ()
help =
    mapM_ putStrLn mesg

mesg :: [String]
mesg =
    [ "Pose a question as an equation such as",
      "    2x + y = 3z, or",
      "    2x = x + y, or",
      "    64x - 41y = a.",
      "The agum programs shows the result of unification and matching.",
      "",
      "The unification problem is given two terms t and t', find a most",
      "general unifier s such that s(t) = s(t').  The matching problem",
      "for terms t and t' is to find a most general matcher s such that",
      "s(t) = t'.",
      "",
      ":quit quits the program, :? and :help print this message."]
