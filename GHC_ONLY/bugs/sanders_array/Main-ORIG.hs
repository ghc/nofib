{-
From: Paul Sanders <psanders@srd.bt.co.uk>
To: partain
Subject: A puzzle for you
Date: Mon, 28 Oct 91 17:02:19 GMT

I'm struggling with the following code fragment at the moment:
-}

conv_list :: [a] -> [b] -> [[c]] -> Array (a,b) c -> Array (a,b) c
conv_list [] _ _ ar = ar
conv_list _ _ [] ar = ar
conv_list (r:rs) cls (rt:rts) ar
      = conv_list rs cls rts ar'
        where ar' = conv_elems r cls rt ar

conv_elems :: a -> [b] -> [c] -> Array (a,b) c -> Array (a,b) c
conv_elems row [] _ ar = ar
conv_elems _ _ [] ar = ar
conv_elems row (col:cls) (rt:rts) ar
      = conv_elems row cls rts ar'
        where ar' = ar // ((row,col) := rt)

ar_list = [[1,2,3],
           [6,7,8],
           [10,12,15]]

ar :: Array (Int, Int) Int
ar = conv_list [1..3] [1..3] ar_list init_ar
     where init_ar = array ((1,1),(3,3)) []

main = appendChan stdout (show ar) abort done

{-
What it tries to do is turn a list of lists into a 2-d array in an incremental
fashion using 2 nested for-loops. It compiles okay on the prototype compiler
but gives a segmentation fault when it executes. I know I can define in the
array in one go (and I have done) but, for my piece of mind, I want to get this
way working properly.

Is it a bug in the prototype or is there a glaringly obvious error in my code
which I've been stupid to spot ????

Hoping its the latter,

Paul.
-}
