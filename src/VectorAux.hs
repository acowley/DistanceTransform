{-# LANGUAGE BangPatterns #-}
module VectorAux where
import Data.Vector.Generic (Vector, create)
import Data.Vector.Generic.Mutable (new, unsafeWrite)

-- Like Vector's unfoldrN, but the unfolding function should be
-- total. In practice, this means that a new seed must be generated
-- when producing the last required value of the Vector. This seed
-- will never be used, but some value must be supplied.
unfoldN :: Vector v a => Int -> (b -> (a,b)) -> b -> v a
unfoldN n f z = create $ do v <- new n
                            let go !i !s
                                  | i == n = return v
                                  | otherwise = let (x,s') = f s
                                                in unsafeWrite v i x >>
                                                   go (i+1) s'
                            go 0 z
{-# INLINE unfoldN #-}

-- Pass the function being unfolded the index of the entry as well as
-- the current seed.
iunfoldN :: Vector v a => Int -> (Int -> b -> (a,b)) -> b -> v a
iunfoldN n f z = create $ do v <- new n
                             let go !i !s
                                   | i == n = return v
                                   | otherwise = let (x,s') = f i s
                                                 in unsafeWrite v i x >>
                                                    go (i+1) s'
                             go 0 z
{-# INLINE iunfoldN #-}
