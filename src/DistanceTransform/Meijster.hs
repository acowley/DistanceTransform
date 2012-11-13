{-# LANGUAGE BangPatterns, RecordWildCards #-}
module DistanceTransform.Meijster (edt, edtPar) where
import Control.Applicative
import Control.Monad (when, foldM)
import Control.Monad.Primitive (PrimMonad, PrimState)
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
     let f !offset !step = let gsq !i = sedt ! (offset+step*i)
                               dt !i = vwrite (offset + i*step)
                           in do temps <- prepTemps m gsq
                                 q <- foldM (scan3 m temps) 0 [1..m-1]
                                 foldM (scan4 dt temps) q [m-1,m-2..0]
                                 return ()
     zipFoldMAsYouDo dim f
     return v
  where m = focus dim

parPhaseN :: Zipper Int -> Vector Int -> Vector Int
parPhaseN dim sedt = 
  V.create $ 
  do (v,vread,vwrite) <- newRW $ V.length sedt
     let f offset step = let gsq !i = sedt ! (offset+step*i)
                             dt !i = vwrite (offset + i*step)
                         in do temps <- prepTemps m gsq
                               q <- foldM (scan3 m temps) 0 [1..m-1]
                               foldM (scan4 dt temps) q [m-1,m-2..0]
                               return ()
     unsafeIOToST $ parZipFoldMAsYouDo dim ((unsafeSTToIO .) . f)
     return v
  where m = focus dim

iterateWhileM :: Monad m => (a -> m (Maybe a)) -> a -> m a
iterateWhileM f = go 
  where go x = do !x' <- f x
                  case x' of
                    Nothing -> return x
                    Just x'' -> go x''
  --where go !x = f x >>= maybe (return x) go
{-# INLINE iterateWhileM #-}

-- Data structure for holding accessors for temporary arrays used for
-- each row in phase 2 of the algorithm.
data Temps m = Temps { swrite  :: Int -> Int -> m ()
                     , sread   :: Int -> m Int
                     , twrite  :: Int -> Int -> m ()
                     , tread   :: Int -> m Int
                     , fMetric :: Int -> Int -> Int 
                     , sep     :: Int -> Int -> Int
                     , tmpGsq  :: Int -> Int }

-- Initialize temporary arrays and partially applied per-row functions
-- f and sep.
prepTemps :: PrimMonad m => Int -> (Int -> Int) -> m (Temps m)
prepTemps m gsq = do (_, sread, swrite) <- newRW m
                     (_, tread, twrite) <- newRW m
                     swrite 0 0
                     twrite 0 0
                     return $ Temps swrite sread twrite tread f sep gsq
  where f !x !i = let !d = x - i in d*d + gsq i
        sep !i !u = (u*u - i*i + gsq u - gsq i) `quot` 2*(u-i)
{-# INLINE prepTemps #-}

-- Phase N forward scan across a row.
scan3 :: (Applicative m, Monad m) => Int -> Temps m -> Int -> Int -> m Int
scan3 m Temps{..} !q0 !u =
  do -- !q <- ({-# SCC "iterateWhileM_qaux" #-} iterateWhileM qaux q0)
     !q <- ({-# SCC "qaux_self" #-} qaux' q0)
     if q < 0
     then swrite 0 u >> return 0
     else do -- !w <- (1+) <$> (sep <$> sread q <*> pure u)
             !w <- sread q >>= return . flip sep' u
             if w < m
             then let !q' = q+1
                  in do swrite q' u
                        twrite q' w
                        return q'
             else return q
  where qaux' !q
          | q < 0 = return q
          | otherwise = do !tq <- tread q
                           !sq <- sread q
                           if fMetric' tq sq > fMetric' tq u
                           then qaux' (q-1)
                           else return q
        qaux !q 
          | q < 0 = return Nothing
          | otherwise = do !tq <- tread q
                           !sq <- sread q
                           return $ if fMetric tq sq > fMetric tq u
                                    then let !q' = q - 1 in Just q'
                                    else Nothing
        fMetric' !x !i = let !d = x - i in d*d + tmpGsq i
        sep' !i !u = ((u*u - i*i + tmpGsq u - tmpGsq i) `quot` 2*(u-i)) + 1
{-# INLINE scan3 #-}

-- Phase N backward scan across a row.
scan4 :: Monad m => (Int -> Int -> m ()) -> Temps m -> Int -> Int -> m Int
scan4 dtwrite Temps{..} !q !u = 
  do sq <- sread q
     dtwrite u (fMetric u sq)
     tq <- tread q
     if u == tq then return (q-1) else return q
{-# INLINE scan4 #-}

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
