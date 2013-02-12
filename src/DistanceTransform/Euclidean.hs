{-# LANGUAGE BangPatterns, RecordWildCards, ScopedTypeVariables, 
             FlexibleContexts #-}
module DistanceTransform.Euclidean (edt, edtPar, sedt, sedtPar) where
import Control.Monad (when)
import Control.Monad.ST (ST)
import Control.Monad.ST.Unsafe (unsafeIOToST, unsafeSTToIO)
import qualified Data.Vector.Generic as G
import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Data.Word (Word8)
import DistanceTransform.Internal.Indexer

-- This constructs Meijster's G function.
phase1 :: (Integral a, G.Vector v a) => Zipper Int -> v a -> Vector Int
phase1 dim p = V.map (\x -> x*x) $ V.create $
               do v <- VM.new (product $ fromZipper dim)
                  let pullRight !i = if p G.! i == 0
                                     then VM.unsafeWrite v i 0
                                     else VM.unsafeRead v (i-step) >>= 
                                          (VM.unsafeWrite v i $!) . (1+)
                      pushLeft !i = do !prev <- VM.unsafeRead v (i+step)
                                       !curr <- VM.unsafeRead v i
                                       when (prev < curr) 
                                            (VM.unsafeWrite v i $! prev+1)
                      innerLoop !offset _ = 
                        do VM.unsafeWrite v offset $! toInfty offset
                           mapM_ (pullRight . (offset+)) [step,2*step..n' - 1]
                           mapM_ (pushLeft . (offset+)) [n'-2*step,n'-3*step..0]
                  zipFoldMAsYouDo dim innerLoop
                  return v
  where toInfty !i = let !dimsum = zipSum dim
                     in if p G.! i == 0 then 0 else dimsum
        {-# INLINE toInfty #-}
        step = zipStep dim
        n = focus dim -- Get the actual dimension size
        n' = n * step

parPhase1 :: (Integral a, G.Vector v a) => Zipper Int -> v a -> Vector Int
parPhase1 dim p = V.map (\x -> x*x) $ V.create $
               do v <- VM.new (product $ fromZipper dim)
                  let pullRight !i = if p G.! i == 0
                                     then VM.unsafeWrite v i 0
                                     else VM.unsafeRead v (i-step) >>= 
                                          (VM.unsafeWrite v i $!) . (1+)
                      pushLeft !i = do !prev <- VM.unsafeRead v (i+step)
                                       !curr <- VM.unsafeRead v i
                                       when (prev < curr) 
                                            (VM.unsafeWrite v i $! prev+1)
                      innerLoop !offset _ = 
                        do VM.unsafeWrite v offset $! toInfty offset
                           mapM_ (pullRight . (offset+)) [step,2*step..n' - 1]
                           mapM_ (pushLeft . (offset+)) [n'-2*step,n'-3*step..0]
                  unsafeIOToST $ 
                    parZipFoldMAsYouDo dim ((unsafeSTToIO .) . innerLoop)
                  return v
  where toInfty !i = let dimsum = zipSum dim
                     in if p G.! i == 0 then 0 else dimsum
        {-# INLINE toInfty #-}
        step = zipStep dim
        n = focus dim -- Get the actual dimension size
        n' = n * step

-- Each phase needs the squared eucilidean distance from the previous
-- phase.
phaseN :: Zipper Int -> Vector Int -> Vector Int
phaseN dim sedt' = 
  V.create $
  do v <- VM.new $ V.length sedt'
     zipFoldMAsYouDo dim (phaseNRow m sedt' v)
     return v
  where m = focus dim

phaseNRow :: forall s. Int -> Vector Int -> VM.STVector s Int -> Int -> Int
          -> ST s ()
phaseNRow m sedt' v offset step = 
  do s <- VM.new m
     t <- VM.new m
     let {-# INLINE fMetric #-}
         fMetric !x !i = let !d = x - i in d*d + gsq i
         {-# INLINE sep #-}
         -- I flipped the order of the arguments from Meijster's paper
         -- for ease of use in scan3
         sep !u !i = ((u*u-i*i+gsq u - gsq i) `quot` (2*(u-i))) + 1
     VM.unsafeWrite s 0 0
     VM.unsafeWrite t 0 0
     let {-# INLINE qaux #-}
         -- qaux :: Int -> (Int -> ST s Int) -> Int -> ST s Int
         qaux !u k = goqaux
           where goqaux !q | q < 0 = k q
                           | otherwise = do !tq <- VM.unsafeRead t q
                                            !sq <- VM.unsafeRead s q
                                            if fMetric tq sq > fMetric tq u
                                            then let !q' = q-1 in goqaux q'
                                            else k q
         scan3 !q0 !u = let {-# INLINE aux #-}
                            aux !q = 
                              if q < 0 
                              then VM.unsafeWrite s 0 u >> return 0
                              else do !w <- (sep u $!) `fmap` VM.unsafeRead s q
                                      if w < m
                                      then let !q' = q+1
                                           in do VM.unsafeWrite s q' u
                                                 VM.unsafeWrite t q' w
                                                 return q'
                                      else return q
                        in qaux u aux q0
         scan4 !q !u = do !sq <- VM.unsafeRead s q
                          let !i = offset + u * step
                          VM.unsafeWrite v i $! fMetric u sq
                          !tq <- VM.unsafeRead t q
                          if u == tq then let !q' = q-1 in return q' 
                                     else return q
     q <- foldMfromStepTo scan3 (0::Int) 1 (+1) (m-1)
     _ <- foldMfromStepTo scan4 q (m-1) (subtract 1) (0::Int)
     return ()
  where gsq !i = sedt' ! (offset+step*i)
        {-# INLINE gsq #-}

{-# INLINE foldMfromStepTo #-}
foldMfromStepTo :: (Eq b, Monad m) => 
                   (a -> b -> m a) -> a -> b -> (b -> b) -> b -> m a
foldMfromStepTo f z from step to = go from z
  where to' = step to
        go !x !acc = if x == to' then return acc else f acc x >>= go (step x)

parPhaseN :: Zipper Int -> Vector Int -> Vector Int
parPhaseN dim sedt' = 
  V.create $ 
  do v <- VM.new $ V.length sedt'
     unsafeIOToST $ parZipFoldMAsYouDo dim ((unsafeSTToIO .) . phaseNRow m sedt' v)
     return v
  where m = focus dim

-- |Dimensions given as [width,height,depth...]. The left-most
-- dimension is the inner-most.
sedt :: (G.Vector v a, Integral a) => [Int] -> v a -> Vector Int
sedt dims p = go (left dim0) (phase1 dim0 p)
  where dim0 = rightmost . unsafeToZipper $ reverse dims
        go Nothing sedt' = sedt'
        go (Just dim) sedt' = go (left dim) (phaseN dim sedt')
{-# INLINE sedt #-}

-- |Compute the Euclidean distance transform of an N-dimensional
-- array. Dimensions given as [width,height,depth...]. The left-most
-- dimension is the inner-most. For an array representing a 2D
-- collection in row-major format, we would give [width,height] or
-- [columns,rows].
edt :: (Integral a, Floating b, G.Vector v a, G.Vector v b, G.Vector v Int)
    => [Int] -> v a -> v b
edt dims v = G.map aux . G.convert $ sedt dims v
  where aux = sqrt . fromIntegral -- . min 80
{-# INLINE edt #-}

-- |Compute the Euclidean distance transform of an N-dimensional array
-- using multiple processor cores. Dimensions given as
-- [width,height,depth...]. The left-most dimension is the
-- inner-most. For an array representing a 2D collection in row-major
-- format, we would give [width,height] or [columns,rows].
edtPar :: (Integral a, Floating b, G.Vector v a, G.Vector v b, G.Vector v Int)
       => [Int] -> v a -> v b
edtPar dims v = G.map aux . G.convert $ sedtPar dims v
  where aux = sqrt . fromIntegral -- . min 80
{-# INLINE edtPar #-}

sedtPar :: (G.Vector v a, Integral a) => [Int] -> v a -> Vector Int
sedtPar dims p = go (left dim0) (parPhase1 dim0 p)
  where dim0 = rightmost . unsafeToZipper $ reverse dims
        go Nothing sedt' = sedt'
        go (Just dim) sedt' = go (left dim) (parPhaseN dim sedt')
{-# INLINE sedtPar #-}