succ :: Int -> Int
succ x = x + 1

isdivs :: Int  -> Int -> Bool
isdivs n x = mod x n /= 0

the_filter :: [Int] -> [Int]
the_filter (n:ns) = filter (isdivs n) ns

primes :: [Int]
primes = map head (iterate the_filter (iterate succ 2))

main = print (primes !! 1500)
--OLD: main = print (take 300 primes)
