{-# LANGUAGE BangPatterns, FlexibleContexts, ScopedTypeVariables #-}
-- |N-dimensional parallel Euclidean distance transform using an
-- approach derived from: Meijster et al., /"A general algorithm for
-- computing distance transforms in linear time."/
module DistanceTransform.Euclidean (edt, edtPar, sedt, sedtPar) where
import Control.Monad (when)
import Control.Monad.ST (ST)
import Control.Monad.ST.Unsafe (unsafeIOToST, unsafeSTToIO)
import Data.Vector.Generic (Vector, (!))
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as VM
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word8)
import DistanceTransform.Internal.Indexer

-- This constructs Meijster's G function.
phase1 :: (Integral a, Vector v a, Vector v Int) => Zipper Int -> v a -> v Int
phase1 dim p = V.map (\x -> x*x) $ V.create $
               do v <- VM.new (product $ fromZipper dim)
                  let pullRight !i = if p ! i == 0
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
                     in if p ! i == 0 then 0 else dimsum
        {-# INLINE toInfty #-}
        step = zipStep dim
        n = focus dim -- Get the actual dimension size
        n' = n * step
{-# SPECIALIZE phase1 :: Zipper Int -> U.Vector Int -> U.Vector Int #-}
{-# SPECIALIZE phase1 :: Zipper Int -> U.Vector Word8 -> U.Vector Int #-}
{-# SPECIALIZE phase1 :: Zipper Int -> S.Vector Int -> S.Vector Int #-}
{-# SPECIALIZE phase1 :: Zipper Int -> S.Vector Word8 -> S.Vector Int #-}

parPhase1 :: (Integral a, Vector v a, Vector v Int) => Zipper Int -> v a -> v Int
parPhase1 dim p = V.map (\x -> x*x) $ V.create $
               do v <- VM.new (product $ fromZipper dim)
                  let pullRight !i = if p ! i == 0
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
                     in if p ! i == 0 then 0 else dimsum
        {-# INLINE toInfty #-}
        step = zipStep dim
        n = focus dim -- Get the actual dimension size
        n' = n * step
{-# SPECIALIZE parPhase1 :: Zipper Int -> U.Vector Int -> U.Vector Int #-}
{-# SPECIALIZE parPhase1 :: Zipper Int -> U.Vector Word8 -> U.Vector Int #-}
{-# SPECIALIZE parPhase1 :: Zipper Int -> S.Vector Int -> S.Vector Int #-}
{-# SPECIALIZE parPhase1 :: Zipper Int -> S.Vector Word8 -> S.Vector Int #-}

-- Each phase needs the squared eucilidean distance from the previous
-- phase.
phaseN :: Vector v Int => Zipper Int -> v Int -> v Int
phaseN dim sedt' = 
  V.create $
  do v <- VM.new $ V.length sedt'
     zipFoldMAsYouDo dim (phaseNRow m sedt' v)
     return v
  where m = focus dim
{-# SPECIALIZE phaseN :: Zipper Int -> U.Vector Int -> U.Vector Int #-}
{-# SPECIALIZE phaseN :: Zipper Int -> S.Vector Int -> S.Vector Int #-}

phaseNRow :: forall v mv s. (Vector v Int, VM.MVector mv Int)
          => Int -> v Int -> mv s Int -> Int -> Int -> ST s ()
phaseNRow m sedt' v offset step = 
  do s <- UM.new m
     t <- UM.new m
     let {-# INLINE fMetric #-}
         fMetric !x !i = let !d = x - i in d*d + gsq i
         {-# INLINE sep #-}
         -- I flipped the order of the arguments from Meijster's paper
         -- for ease of use in scan3
         sep !u !i = ((u*u-i*i+gsq u - gsq i) `quot` (2*(u-i))) + 1
     VM.unsafeWrite s 0 0
     VM.unsafeWrite t 0 0
     let {-# INLINE qaux #-}
         qaux :: Int -> (Int -> ST s Int) -> Int -> ST s Int
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
{-# SPECIALIZE phaseNRow :: Int -> U.Vector Int -> U.MVector s Int -> Int -> Int -> ST s () #-}
{-# SPECIALIZE phaseNRow :: Int -> S.Vector Int -> S.MVector s Int -> Int -> Int -> ST s () #-}

foldMfromStepTo :: (Eq b, Monad m) => 
                   (a -> b -> m a) -> a -> b -> (b -> b) -> b -> m a
foldMfromStepTo f z from step to = go from z
  where to' = step to
        go !x !acc = if x == to' then return acc else f acc x >>= go (step x)
{-# INLINE foldMfromStepTo #-}

parPhaseN :: Vector v Int => Zipper Int -> v Int -> v Int
parPhaseN dim sedt' = 
  V.create $ 
  do v <- VM.new $ V.length sedt'
     unsafeIOToST $ parZipFoldMAsYouDo dim ((unsafeSTToIO .) . phaseNRow m sedt' v)
     return v
  where m = focus dim
{-# SPECIALIZE parPhaseN :: Zipper Int -> U.Vector Int -> U.Vector Int #-}
{-# SPECIALIZE parPhaseN :: Zipper Int -> S.Vector Int -> S.Vector Int #-}

-- |Compute the squared Euclidean distance transform of an
-- N-dimensional array. Dimensions given as
-- @[width,height,depth...]@. The left-most dimension is the
-- inner-most.
sedt :: (Vector v a, Vector v Int, Integral a) => [Int] -> v a -> v Int
sedt dims p = go (left dim0) (phase1 dim0 p)
  where dim0 = rightmost . unsafeToZipper $ reverse dims
        go Nothing sedt' = sedt'
        go (Just dim) sedt' = go (left dim) (phaseN dim sedt')
{-# SPECIALIZE sedtPar :: [Int] -> U.Vector Int -> U.Vector Int #-}
{-# SPECIALIZE sedtPar :: [Int] -> U.Vector Word8 -> U.Vector Int #-}
{-# SPECIALIZE sedtPar :: [Int] -> S.Vector Int -> S.Vector Int #-}
{-# SPECIALIZE sedtPar :: [Int] -> S.Vector Word8 -> S.Vector Int #-}

-- |Compute the Euclidean distance transform of an N-dimensional
-- array. Dimensions given as @[width,height,depth...]@. The left-most
-- dimension is the inner-most. For an array representing a 2D
-- collection in row-major format, we would give @[width,height]@ or
-- @[columns,rows]@.
edt :: (Integral a, Floating b, Vector v a, Vector v b, Vector v Int)
    => [Int] -> v a -> v b
edt dims v = V.map aux $ sedt dims v
  where aux = sqrt . fromIntegral
{-# SPECIALIZE edt :: [Int] -> U.Vector Int -> U.Vector Float #-}
{-# SPECIALIZE edt :: [Int] -> U.Vector Int -> U.Vector Double #-}
{-# SPECIALIZE edt :: [Int] -> U.Vector Word8 -> U.Vector Float #-}
{-# SPECIALIZE edt :: [Int] -> U.Vector Word8 -> U.Vector Double #-}
{-# SPECIALIZE edt :: [Int] -> S.Vector Int -> S.Vector Float #-}
{-# SPECIALIZE edt :: [Int] -> S.Vector Int -> S.Vector Double #-}
{-# SPECIALIZE edt :: [Int] -> S.Vector Word8 -> S.Vector Float #-}
{-# SPECIALIZE edt :: [Int] -> S.Vector Word8 -> S.Vector Double #-}

-- |Compute the Euclidean distance transform of an N-dimensional array
-- using multiple processor cores. Dimensions given as
-- @[width,height,depth...]@. The left-most dimension is the
-- inner-most. For an array representing a 2D collection in row-major
-- format, we would give @[width,height]@ or @[columns,rows]@.
edtPar :: (Integral a, Floating b, Vector v a, Vector v b, Vector v Int)
       => [Int] -> v a -> v b
edtPar dims v = V.map aux $ sedtPar dims v
  where aux = sqrt . fromIntegral
{-# SPECIALIZE edtPar :: [Int] -> U.Vector Int -> U.Vector Float #-}
{-# SPECIALIZE edtPar :: [Int] -> U.Vector Int -> U.Vector Double #-}
{-# SPECIALIZE edtPar :: [Int] -> U.Vector Word8 -> U.Vector Float #-}
{-# SPECIALIZE edtPar :: [Int] -> U.Vector Word8 -> U.Vector Double #-}
{-# SPECIALIZE edtPar :: [Int] -> S.Vector Int -> S.Vector Float #-}
{-# SPECIALIZE edtPar :: [Int] -> S.Vector Int -> S.Vector Double #-}
{-# SPECIALIZE edtPar :: [Int] -> S.Vector Word8 -> S.Vector Float #-}
{-# SPECIALIZE edtPar :: [Int] -> S.Vector Word8 -> S.Vector Double #-}

-- |Compute the squared Euclidean distance transform of an
-- N-dimensional array using multiple processor cores. Dimensions
-- given as @[width,height,depth...]@. The left-most dimension is the
-- inner-most.
sedtPar :: (Vector v a, Vector v Int, Integral a) => [Int] -> v a -> v Int
sedtPar dims p = go (left dim0) (parPhase1 dim0 p)
  where dim0 = rightmost . unsafeToZipper $ reverse dims
        go Nothing sedt' = sedt'
        go (Just dim) sedt' = go (left dim) (parPhaseN dim sedt')
{-# SPECIALIZE sedtPar :: [Int] -> U.Vector Int -> U.Vector Int #-}
{-# SPECIALIZE sedtPar :: [Int] -> U.Vector Word8 -> U.Vector Int #-}
{-# SPECIALIZE sedtPar :: [Int] -> S.Vector Int -> S.Vector Int #-}
{-# SPECIALIZE sedtPar :: [Int] -> S.Vector Word8 -> S.Vector Int #-}
