{-# LANGUAGE BangPatterns, RecordWildCards, ScopedTypeVariables #-}
module DistanceTransform.Meijster (edt, edtPar) where
import Control.Applicative
import Control.Monad (when, foldM)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (ST)
import Control.Monad.ST.Unsafe (unsafeIOToST, unsafeSTToIO)
import Data.Vector.Storable (Vector, (!))
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import DistanceTransform.Indexer

-- Allocate a mutable vector, return the vector along with read and
-- write functions.
newRW :: PrimMonad m => Int -> 
         m (VM.MVector (PrimState m) Int, Int -> m Int, Int -> Int -> m ())
newRW n = do { v <- VM.new n; return (v, VM.unsafeRead v, VM.unsafeWrite v) }

-- This constructs Meijster's G function.
phase1 :: Zipper Int -> Vector Int -> Vector Int
phase1 dim p = V.map (\x -> x*x) $ V.create $
               do (v, vread, vwrite) <- newRW (product $ fromZipper dim)
                  let pullRight !i = if p ! i == 0
                                     then vwrite i 0
                                     else vread (i-step) >>= vwrite i . (1+)
                      pushLeft !i = do prev <- vread (i+step)
                                       curr <- vread i
                                       when (prev < curr) (vwrite i $! prev+1)
                      innerLoop !offset _ = 
                        do vwrite offset $ toInfty offset
                           mapM_ (pullRight . (offset+)) [step,2*step..n' - 1]
                           mapM_ (pushLeft . (offset+)) [n'-2*step,n'-3*step..0]
                  zipFoldMAsYouDo dim innerLoop
                  return v
  where toInfty i = let dimsum = zipSum dim
                    in if p ! i == 0 then 0 else dimsum
        step = zipStep dim
        n = focus dim -- Get the actual dimension size
        n' = n * step

-- Each phase needs the squared eucilidean distance from the previous
-- phase.
phaseN :: Zipper Int -> Vector Int -> Vector Int
phaseN dim sedt = 
  V.create $
  do (v,vread,vwrite) <- newRW $ V.length sedt
     zipFoldMAsYouDo dim (phaseNRow m sedt v)
     return v
  where m = focus dim

phaseNRow :: forall s. Int -> Vector Int -> VM.MVector s Int -> Int -> Int -> ST s ()
phaseNRow m sedt v offset step = 
  do s <- VM.new m
     t <- VM.new m
     let {-# INLINE swrite #-}
         swrite = VM.unsafeWrite s
         {-# INLINE sread #-}
         sread = VM.unsafeRead s
         {-# INLINE twrite #-}
         twrite = VM.unsafeWrite t
         {-# INLINE tread #-}
         tread = VM.unsafeRead t
         {-# INLINE gsq #-}
         gsq !i = sedt ! (i*step+offset)
         {-# INLINE fMetric #-}
         -- fMetric !x !i = let !d = x - i
         --                     !g = gsq i
         --                 in d*d + g
         fMetric !x !i = let !d = x - i in d*d + gsq i
         {-# INLINE sep #-}
         sep !i !u = let !num = u*u-i*i+gsq u - gsq i
                         !den = 2 * (u - i)
                         !r = (num `quot` den) + 1
                     in r
     swrite 0 0
     twrite 0 0
     let {-# INLINE qaux #-}
         qaux :: Int -> Int -> ST s Int
         qaux !u = goqaux
           where goqaux !q | q < 0 = return q
                           | otherwise = do !tq <- tread q
                                            !sq <- sread q
                                            if fMetric tq sq > fMetric tq u
                                            then let !q' = q-1 in goqaux q'
                                            else return q
         scan3 !q0 !u = do !q <- qaux u q0
                           if q < 0 then swrite 0 u >> return 0
                           else do !w <- sread q >>= return . flip sep u
                                   if w < m
                                   then let !q' = q+1
                                        in do swrite q' u
                                              twrite q' w
                                              return q'
                                   else return q
         scan4 !q !u = do !sq <- sread q
                          let !i = offset + u * step
                          VM.unsafeWrite v i $! fMetric u sq
                          !tq <- tread q
                          if u == tq then let !q' = q-1 in return q' 
                                     else return q
     q <- foldM' scan3 0 [1..m-1]
     foldM' scan4 q [m-1,m-2..0]
     return ()
  where gsq !i = sedt ! (offset+step*i)

{-# INLINE foldM' #-}
foldM' :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldM' f = go
  where go !acc [] = return acc
        go !acc (!x:xs) = f acc x >>= flip go xs

parPhaseN :: Zipper Int -> Vector Int -> Vector Int
parPhaseN dim sedt = 
  V.create $ 
  do (v,vread,vwrite) <- newRW $ V.length sedt
     unsafeIOToST $ parZipFoldMAsYouDo dim ((unsafeSTToIO .) . phaseNRow m sedt v)
     return v
  where m = focus dim

-- |Dimensions given as [width,height,depth...]. The left-most
-- dimension is the inner-most.
sedt :: [Int] -> Vector Int -> Vector Int
sedt dims p = go (left dim0) (phase1 dim0 p)
  where dim0 = rightmost . unsafeToZipper $ reverse dims
        go Nothing sedt = sedt
        go (Just dim) sedt = go (left dim) (phaseN dim sedt)

-- |Compute the Euclidean distance transform of an N-dimensional
-- array. Dimensions given as [width,height,depth...]. The left-most
-- dimension is the inner-most. For an array representing a 2D
-- collection in row-major format, we would give [width,height] or
-- [columns,rows].
edt :: [Int] -> Vector Int -> Vector Float
edt dims v = V.map aux $ sedt dims v
  where aux = sqrt . fromIntegral . min 80

edtPar :: [Int] -> Vector Int -> Vector Float
edtPar dims v = V.map aux $ sedtPar dims v
  where aux = sqrt . fromIntegral . min 80

sedtPar :: [Int] -> Vector Int -> Vector Int
sedtPar dims p = go (left dim0) (phase1 dim0 p)
  where dim0 = rightmost . unsafeToZipper $ reverse dims
        go Nothing sedt = sedt
        go (Just dim) sedt = go (left dim) (parPhaseN dim sedt)
