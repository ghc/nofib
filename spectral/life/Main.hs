--------------------------------
--	The Game of Life      --
--			      --
--    (from John Launchbury)  --
--------------------------------

main inp = [AppendChan stdout ("\FF" ++ life 30 start)]

start :: [[Int]]
start = [[],[],[],[],[],[],[],[],[],[],[],[],[],[],
         [0,0,0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0]]


-- Claculating the next generation

gen n board = map row (shift (copy n 0) board)

row (last,this,next)
  = zipWith3 elt (shift 0 last) (shift 0 this) (shift 0 next)

elt (a,b,c) (d,e,f) (g,h,i) | tot < 2 || tot > 3 = 0
                            | tot == 3           = 1
                            | otherwise          = e
  where tot = a+b+c+d+f+g+h+i
                            
shiftr x xs = [x] ++ init xs
shiftl x xs = tail xs ++ [x]
shift x xs = zip3 (shiftr x xs) xs (shiftl x xs)

copy 0 x = []
copy n x = x : copy (n-1) x

-- Displaying one generation

disp (gen,xss) = gen ++"\n\n"++(foldr (glue "\n") "" . map (concat . map star)) xss

star 0 = "  "
star 1 = " o"
glue s xs ys = xs ++ s ++ ys


-- Generating and displaying a sequence of generations

life n = foldr1 (glue (copy (n+2) '\VT')) . map disp
           . zip (map show [0..]) . limit . iterate (gen n) . initial n

initial n xss = take n (map (take n.(++(copy n 0))) xss ++ copy n (copy n 0))

limit (x:y:xs) | x==y      = [x]
               | otherwise = x : limit (y:xs)
