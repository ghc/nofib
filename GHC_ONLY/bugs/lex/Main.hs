module Main where

main = interact ( \ s -> shows (lex' s) "\n")
     where lex' "" = []
	   lex' s = tok : lex' s' where [(tok,s')] = lex s
