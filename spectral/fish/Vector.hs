module Vector
where


-- A vector is a pair of floats

type Nr = Int
type Vec = (Nr, Nr)

(x1, y1) `vec_add` (x2, y2) = (x1+x2, y1+y2)

vec_add :: Vec -> Vec -> Vec
--  This adds two vectors.

(x1, y1) `vec_sub` (x2, y2) = (x1-x2, y1-y2)

vec_sub :: Vec -> Vec -> Vec
 -- This substracts the second vector from the first.

scale_vec (x, y) factor = (factor*x, factor*y)

scale_vec :: Vec -> Nr -> Vec
-- This performs scalar multiplication of the vector by a (Nr) factor.

scale_vec2 (x, y) a b = ((x*a) `div` b, (y*a) `div` b)

-- This function is provided for efficiency.
-- The first argument is vector.
-- The second argument and third arguments are integers.
-- These integers represent the nummerator and denominator of a rational
-- number which is used to scale the given vector.
-- If a' and b' are the Nr versions of a and b, then this function
-- computes:
--    (x*a/b, y*a/b).

