> module Main where

> import StringMatch (stringMatch)

> main :: Dialogue
> main = getArgs exit parse_args

> parse_args :: [String] -> Dialogue
> parse_args (regexp: files) =
> 	let acc = stringMatch regexp
> 	    acc' = unlines . filter acc . lines
> 	in
> 	    readChan stdin exit (\inp -> 
> 	    appendChan stdout (acc' inp) exit done)
> parse_args _ = done
