module Fun_geom (grid, nil, rot, beside, above, squarelimit)
where
import Vector
import Fish_lines

type Line_segment = (Nr, Nr, Nr, Nr)
type Picture = Vec -> Vec -> Vec -> [Line_segment]

nil a b c = []

grid :: Nr -> Nr -> [Line_segment] -> Vec -> Vec -> Vec
	-> [Line_segment]
grid m n segments a b c
 = [tup2
   (a `vec_add` (scale_vec2 b x0 m) `vec_add` (scale_vec2 c y0 n))  
   (a `vec_add` (scale_vec2 b x1 m) `vec_add` (scale_vec2 c y1 n)) 
	| (x0, y0, x1, y1) <- segments]

rot p a b c = p (a `vec_add` b) c ((0, 0) `vec_sub` b) 

beside m n p q a b c
    = p a (scale_vec2 b m (m+n)) c ++
      q (a `vec_add` (scale_vec2 b m (m+n))) (scale_vec2 b n (n+m)) c

above m n p q a b c
    = p (a `vec_add` (scale_vec2 c n (m+n))) b (scale_vec2 c m (n+m)) ++
      q a b (scale_vec2 c n (m+n))

tup2 :: (a, b) -> (c, d) -> (a, b, c, d)
tup2 (a, b) (c, d) = (a, b, c, d)

tile_to_grid = grid 16 16

p = tile_to_grid p_tile

q = tile_to_grid q_tile

r = tile_to_grid r_tile

s = tile_to_grid s_tile

quartet a b c d = above 1 1 (beside 1 1 a b) (beside 1 1 c d)

t = quartet p q r s

cycle' p1 = quartet p1 (rot (rot (rot p1))) (rot p1) (rot (rot p1))
u = cycle' (rot q)
side1 = quartet nil nil (rot t) t
side2 = quartet side1 side1 (rot t) t
corner1 = quartet nil nil nil u
corner2 = quartet corner1 side1 (rot side1) u
pseudocorner = quartet corner2 side2 (rot side2) (rot t)
pseudolimit = cycle' pseudocorner
nonet p1 p2 p3 p4 p5 p6 p7 p8 p9
 = above 1 2 (beside 1 2 p1 (beside 1 1 p2 p3))
   (above 1 1 (beside 1 2 p4 (beside 1 1 p5 p6))
    (beside 1 2 p7 (beside 1 1 p8 p9)))
corner = nonet corner2 side2 side2 (rot side2) u (rot t) (rot side2) (rot t)
	 (rot q)
squarelimit = cycle' corner
