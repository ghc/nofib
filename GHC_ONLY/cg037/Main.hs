-- Andy Gill bug report 95/08:
-- Constant strings with '\0' in them don't work :-
--
main = putStr "hello\0 world"
