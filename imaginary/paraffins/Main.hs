{-  Ensnaffled by SLPJ from MIT
    via RPaul <rpaul@juicy-juice.lcs.mit.edu> 93/08/26.
-}

-- Id Example Program
-- Title: Paraffins
-- Original Author: Steve Heller

-- Needs the following library functions
-- typeof div = I -> I -> I;
-- typeof ceiling = F -> I;
-- typeof float  = I -> F;
-- typeof length = (list *0) -> I;
-- typeof odd? = I -> B;
-- typeof max = I -> I -> I;

module Main where

-- Generation of radicals.

data Radical = H | C Radical Radical Radical

three_partitions m =
  [ (i,j,k) | i <- [0..(div m 3)],
              j <- [i..(div (m-i) 2)],
              k <- [(m - (i+j))]]

remainders [] = []
remainders (r:rs) = (r:rs) : (remainders rs)

radical_generator n = radicals where 
  { radicals = array (0,n)
                  ((0 := [H]):
                   [j := (rads_of_size_n radicals j) | j <- [1..n]])}


-- $[1 1 1 2 4 8 17 39 89 211 507 1238 3057 7639 19241 48865 124906 321198]
test_radicals n = 
  array (0,n) [i := (length (rads!i)) | rads <- [(radical_generator n)],
                                      i    <- [0..n]]

rads_of_size_n radicals n =
  [ (C ri rj rk) | (i,j,k)  <- (three_partitions (n-1)),
                   (ri:ris) <- (remainders (radicals!i)),
                   (rj:rjs) <- (remainders (if (i==j) then (ri:ris) else radicals!j)),
                   rk       <- (if (j==k) then (rj:rjs) else radicals!k)]

-- Generation of paraffins.

data Paraffin = BCP Radical Radical | CCP Radical Radical Radical Radical

bcp_generator radicals n =
  if (odd n) then
    []
  else
    [ (BCP r1 r2) | (r1:r1s) <- (remainders (radicals!(div n 2))),
                    r2       <- (r1:r1s) ]
    
four_partitions m =
  [ (i,j,k,l) | i <- [0..(div m 4)],
                j <- [i..(div (m-i) 3)],
                k <- [(max j (ceiling ((fromInteger m)/(fromInteger 2)) - i - j))..(div (m-i-j) 2)],
                l <- [(m - (i+j+k))]]

ccp_generator radicals n =
  [ (CCP ri rj rk rl) | (i,j,k,l) <- (four_partitions (n-1)),
                        (ri:ris)  <- (remainders (radicals!i)),
                        (rj:rjs)  <- (remainders (if (i==j) then (ri:ris) else radicals!j)),
                        (rk:rks)  <- (remainders (if (j==k) then (rj:rjs) else radicals!k)),
                        rl        <- (if (k==l) then (rk:rks) else radicals!l)]

bcp_until n = let
  { radicals = radical_generator (div n 2)}
  in
    array (1,n) [j := (bcp_generator radicals j) | j <-  [1..n]]

ccp_until n = let
  { radicals = radical_generator (div n 2)}
  in
    array (1,n) [j := (ccp_generator radicals j) | j <- [1..n]]

ccp_at n = let
  { radicals = radical_generator (div n 2)}
  in
    (ccp_generator radicals n)

paraffins_until n = let
  { radicals = radical_generator (div n 2)}
  in
    array (1,n) [j := ((bcp_generator radicals j),(ccp_generator radicals j)) | j <- [1..n]]

-- $[0 1 0 1 0 3 0 10 0 36 0 153 0 780 0 4005 0]
test_bcp_until n = let
  { result = bcp_until n}
  in

    array (1,n) [i := (length (result!i)) | i <- [1..n]]

-- $[1 0 1 1 3 2 9 8 35 39 159 202 802 1078 4347 6354 24894]
test_ccp_until n = let
  { result = ccp_until n}
  in
    array (1,n) [i := (length (result!i)) | i <- [1..n]]
    
-- $[1 1 1 2 3 5 9 18 35 75 159 355 802 1858 4347 10359 24894]
test_paraffins_until n = let
  { result = paraffins_until n}
  in
    array (1,n) [i := (let { (bv,cv) = result!i} in (length bv) + (length cv))
                      | i <- [1..n]]

main = do
    putStr "Type in N: "
    input <- getContents
    putStr (show (test_paraffins_until (read (head (lines input)))))
