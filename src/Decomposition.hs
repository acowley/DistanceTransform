module Decomposition where
import Data.Vector.Storable (Vector, fromList)

-- 3x3 Euclidean distance transform kernel in row-major order.
k33 :: Vector Float
k33 = fromList [ a1, a0, a1
               , a0,  0, a0
               , a1, a0, a1 ]
  where a0 = -1
        a1 = -sqrt 2

k55 :: Vector Float
k55 = fromList [ b2, b1, b0, b1, b2
               , b1, a1, a0, a1, b1
               , b0, a0,  0, a0, b0
               , b1, a1, a0, a1, b1
               , b2, b1, b0, b1, b2 ]
  where a0 = -1
        a1 = -sqrt 2
        b0 = -2
        b1 = -sqrt 5
        b2 = 2 * a1

-- This is k2 following from equation 12
k2_55 :: Vector Float
k2_55 = fromList [ b2, b1, b0, b1, b2
                 , b1, b0, b1, b0, b1
                 , b0, b1, b0, b1, b0
                 , b1, b0, b1, b0, b1
                 , b2, b1, b0, b1, b2 ]
  where b0 = 2
        b1 = sqrt 5
        b2 = 2 * sqrt 2

-- In Shih, object pixels have value +∞ while background pixels have
-- value 0.

-- Given these kernels with positive distances. An erosion by the
-- kernel is max{k(z)} for all z that are non-zero in the image.

-- The point here is that k55 = min(k33, k2_55), and that
-- decomposition into sums (note that min/max are here reversed from
-- the paper) gives us f ⊖ k55 = max(f ⊖ k33, f ⊖ k2_55)


-- From the initial Coq treatment of morphology
-- Definition erosion (A:t) (B:t) :=
--   fold (fun a s => if for_all (fun b => mem (b ^-^ a) A) B then add a s else s) A empty.
