{-# LANGUAGE BangPatterns #-}
-- |3D Euclidean distance transform.
module DistanceTransform.Euclidean3D (edt) where
import Control.Applicative
import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Vector.Storable (Vector, (!))
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Linear.V3
import DistanceTransform.Internal.Extended
import DistanceTransform.Internal.STNum

-- |3D extension of the two-pass 2D Euclidean distance transform from
-- Shih and Wu, 2004.  The 3x3 neighborhood of each pixel, p, is
-- indexed as
--
-- > q2 q3 q4
-- > q1 p  q5
-- > q8 q7 q6

tripV :: (a,a,a) -> V3 a
tripV (x,y,z) = V3 x y z

forwardN  = V.fromList . map tripV $ frontFace ++ forward2D
backwardN = V.fromList . map tripV $ backFace ++ backward2D

-- Relative indices of neighbors in 3D are broken out into front and
-- back faces with the remaining 8 as in the 2D case.
frontFace, backFace :: [(Int,Int,Int)]
frontFace = [(x,y,-1) | x <- [-1,0,1], y <- [-1,0,1]]
backFace = [(x,y,1) | x <- [-1,0,1], y <- [-1,0,1]]

forward2D, backward2D :: [(Int,Int,Int)]
forward2D = [(x,y,0) | (x,y) <- [(-1,0), (-1,-1), (0,-1), (1,-1)]]
backward2D = [(x,y,0) | (x,y) <- [(1,0), (1,1), (0,1), (-1,1)]]

-- |A 3D array with the same size in each dimension.
data Cube a = Cube Int (Vector a)

-- |Compute a Euclidean distance transform of a 3D array. The array is
-- assumed to represent a cube with the given side length.
edt :: Int -> Vector Int -> Vector Float
edt stride v = V.map aux $ sedt stride v
  where aux :: Extended Int -> Float
        aux = sqrt . fromIntegral . min 80 . unextend 40

mutate :: VM.Storable a => VM.STVector s a -> Int -> (a -> a) -> ST s ()
mutate v i f = VM.unsafeRead v i >>= VM.unsafeWrite v i . f

data Direction = Forward | Backward deriving Eq

edtPass :: Direction -> 
           V.Vector (V3 Int) ->
           Int -> (Int -> V3 Int -> Int) -> 
           VM.STVector s (Extended Int) -> 
           VM.STVector s Int -> -- ^ Rx
           VM.STVector s Int -> -- ^ Ry
           VM.STVector s Int -> -- ^ Rz
           (Int -> V3 Int -> ST s Int) -> ST s ()
edtPass dir ns stride pq f rx ry rz h = go start innerStart innerStart
  where go !i !xi !yi
          | stop i = return ()
          | yi == innerStop = go (step i (2*stride)) innerStart innerStart
          | xi == innerStop = go (step i 2) innerStart (step yi 1)
          | otherwise = let aux q = (+) <$> VM.unsafeRead f (pq i q)
                                        <*> (Finite <$> h i q)
                            update = 
                              do old <- VM.unsafeRead f i
                                 when (old /= 0) $
                                      do qs <- V.mapM aux ns
                                         let j = V.minIndex qs
                                             new = qs ! j
                                         when (new < old) $
                                              let V3 dx dy dz = g $ ns ! j
                                              in do VM.unsafeWrite f i new
                                                    mutate rx i (+ dx)
                                                    mutate ry i (+ dy)
                                                    mutate rz i (+ dz)
                        in update >> go (step i 1) (step xi 1) yi
        (start,stop,innerStart,innerStop,step) = 
          let n = VM.length f
          in if dir == Forward
             then (stride*stride+stride+1, (>= n - stride*stride),
                   1, stride - 1, (+))
             else (VM.length f - stride*stride - 2 - stride, (<= stride*stride),
                   stride - 2, 0, (-))
          
        -- (start,stop,step) = let n = VM.length f
        --                     in if dir == Forward
        --                        then (stride*stride,n-stride*stride,(+1))
        --                        else (n-stride*stride-1,stride*stride-1, subtract 1)

-- edtForward and edtBackward leave a 1 pixel border on each side of
-- the cube.

edtForward :: Int -> (Int -> V3 Int -> Int) -> 
              VM.STVector s (Extended Int) -> 
              VM.STVector s Int -> -- ^ Rx
              VM.STVector s Int -> -- ^ Ry
              VM.STVector s Int -> -- ^ Rz
              (Int -> V3 Int -> ST s Int) -> ST s ()
edtForward stride pq f rx ry rz h = go (stride*stride+stride+1) 1 1
  where go !i !xi !yi
          | i >= stop = return ()
          | yi == innerStop = go (step i (2*stride)) innerStart innerStart
          | xi == innerStop = go (step i 2) innerStart (step yi 1)
          | otherwise = let aux q = (+) <$> VM.unsafeRead f (pq i q)
                                        <*> (Finite <$> h i q)
                            update = 
                              do old <- VM.unsafeRead f i
                                 when (old /= 0) $
                                      do qs <- V.mapM aux forwardN
                                         let j = V.minIndex qs
                                             new = qs ! j
                                         when (new < old) $
                                              let V3 dx dy dz = g $ forwardN ! j
                                              in do VM.unsafeWrite f i new
                                                    mutate rx i (+ dx)
                                                    mutate ry i (+ dy)
                                                    mutate rz i (+ dz)
                        in update >> go (step i 1) (step xi 1) yi
        stop = VM.length f - stride*stride
        innerStop = stride - 1
        innerStart = 1
        step = (+)

edtBackward :: Int -> (Int -> V3 Int -> Int) -> 
               VM.STVector s (Extended Int) -> 
               VM.STVector s Int -> -- ^ Rx
               VM.STVector s Int -> -- ^ Ry
               VM.STVector s Int -> -- ^ Rz
               (Int -> V3 Int -> ST s Int) -> ST s ()
edtBackward stride pq f rx ry rz h = go start (stride-2) (stride-2)
  where go !i !xi !yi
          | i <= stop = return ()
          | yi == 0 = go (i-stride-stride) (stride-2) (stride-2)
          | xi == 0 = go (i-2) (stride-2) (yi-1)
          | otherwise = let aux q = (+) <$> VM.unsafeRead f (pq i q)
                                        <*> (Finite <$> h i q)
                            update = 
                              do old <- VM.unsafeRead f i
                                 when (old /= 0) $
                                      do qs <- V.mapM aux forwardN
                                         let j = V.minIndex qs
                                             new = qs ! j
                                         when (new < old) $
                                              let V3 dx dy dz = g $ forwardN ! j
                                              in do VM.unsafeWrite f i new
                                                    mutate rx i (+ dx)
                                                    mutate ry i (+ dy)
                                                    mutate rz i (+ dz)
                        in update >> go (i-1) (xi-1) yi
        stop = stride*stride
        start = VM.length f - stride*stride - 2 - stride


g :: V3 Int -> V3 Int
g = fmap abs

forward, backward :: Int -> (Int -> V3 Int -> Int) -> 
                     VM.STVector s (Extended Int) ->
                     VM.STVector s Int -> 
                     VM.STVector s Int -> 
                     VM.STVector s Int ->
                     (Int -> V3 Int -> ST s Int) -> ST s ()
forward = edtPass Forward forwardN
backward = edtPass Backward backwardN

-- |Squared Euclidean distance transform.
sedt :: (Ord a, Num a, VM.Storable a) => 
        Int -> Vector a -> Vector (Extended Int)
sedt stride img = V.create $ 
                  do v <- V.thaw (V.map (\x -> if x > 0 then PosInf else 0) img)
                     rxv <- VM.replicate (stride*strideSq) (0::Int)
                     ryv <- VM.replicate (stride*strideSq) (0::Int)
                     rzv <- VM.replicate (stride*strideSq) (0::Int)
                     let rx i = STNum (VM.unsafeRead rxv i)
                         ry i = STNum (VM.unsafeRead ryv i)
                         rz i = STNum (VM.unsafeRead rzv i)
                     let h i (V3 1 0 0) = stnum $ 2 * rx (i+1) + 1
                         h i (V3 _ 0 0) = stnum $ 2 * rx (i-1) + 1
                         h i (V3 0 1 0) = stnum $ 2 * ry (i+stride) + 1
                         h i (V3 0 _ 0) = stnum $ 2 * ry (i-stride) + 1
                         h i (V3 0 0 1) = stnum $ 2 * rz (i+strideSq) + 1
                         h i (V3 0 0 _) = stnum $ 2 * rz (i-strideSq) + 1
                         h i q@(V3 0 _ _) = let j = pq i q
                                            in stnum $ 2 * (ry j + rz j + 1)
                         h i q@(V3 _ 0 _) = let j = pq i q
                                            in stnum $ 2 * (rx j + rz j + 1)
                         h i q@(V3 _ _ 0) = let j = pq i q
                                            in stnum $ 2 * (rx j + ry j + 1)
                         h i q = let j = pq i q
                                 in stnum $ 2 * (rx j + ry j + rz j) + 3
                     forward stride pq v rxv ryv rzv h
                     backward stride pq v rxv ryv rzv h 
                     -- edtForward stride pq v rxv ryv rzv h
                     -- edtBackward stride pq v rxv ryv rzv h 
                     return v
  where -- Compute image index of a neighbor
        pq :: Int -> V3 Int -> Int
        pq i (V3 x y z) = i + x + stride*y + strideSq*z
        strideSq = stride * stride
