import Array -- 1.3
infix 1 =:
(=:) a b = (a,b)

main = putStr (shows a "\n") 
    where
	a :: Array Integer Integer
	a = array (1,100) ((1 =: 1) : [i =: i * a!(i-1) | i <- [2..100]])
