{-# LANGUAGE BangPatterns, RecordWildCards #-}
module DistanceTransform.Meijster2D (edt) where
import Control.Applicative
import Control.Monad (when, foldM)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Vector.Storable (Vector, (!))
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM

-- Function @f@ will be applied sequentially along each row using the
-- given column indices.
rowsM :: Monad m => Int -> Int -> [Int] -> (Int -> m ()) -> m ()
rowsM m n cs f = mapM_ aux [0..n-1]
  where aux r = mapM_ f $ map (r*m +) cs -- [r*m..r*m+m-1]

-- Function @f@ will be applied sequentially down each column using
-- the given row indices.
colsM :: Monad m => Int -> Int -> [Int] -> (Int -> m ()) -> m ()
colsM m n rs f = mapM_ aux [0..m-1]
  where aux c = mapM_ f $ map ((c +) . (m*)) rs -- [c,c+m .. m*(n-1)+c]

-- Allocate a mutable vector, return the vector along with read and
-- write functions.
newRW :: PrimMonad m => Int -> 
         m (VM.MVector (PrimState m) Int, Int -> m Int, Int -> Int -> m ())
newRW n = do { v <- VM.new n; return (v, VM.unsafeRead v, VM.unsafeWrite v) }

-- Build Meijster's G function that finds nearest neighbors down and
-- up columns. This is phase 1 of the distance transform.
prepareG :: Int -> Int -> Vector Int -> Vector Int
prepareG m n p = V.create $ 
                 do (v, vread, vwrite) <- newRW (m*n)
                    let pullDown i = if p ! i == 0
                                     then vwrite i 0
                                     else vread (i-m) >>= vwrite i . (1+)
                        pushUp i = do below <- vread (i+m)
                                      curr <- vread i
                                      when (below < curr)
                                           (vwrite i $! below + 1)
                    mapM_ (\i -> vwrite i $ if p!i == 0 then 0 else m+n) [0..m-1]
                    colsM m n [1..n-1] pullDown
                    colsM m n [n-2,n-3..0] pushUp
                    return v

iterateWhileM :: Monad m => (a -> m (Maybe a)) -> a -> m a
iterateWhileM f = go 
  where go !x = f x >>= maybe (return x) go

-- Data structure for holding accessors for temporary arrays used for
-- each row in phase 2 of the algorithm.
data Temps m = Temps { swrite  :: Int -> Int -> m ()
                     , sread   :: Int -> m Int
                     , twrite  :: Int -> Int -> m ()
                     , tread   :: Int -> m Int
                     , fMetric :: Int -> Int -> Int 
                     , sep     :: Int -> Int -> Int }

-- Initialize temporary arrays and partially applied per-row functions
-- f and sep.
prepTemps :: PrimMonad m => Int -> (Int -> Int) -> m (Temps m)
prepTemps m gsq = do (_, sread, swrite) <- newRW m
                     (_, tread, twrite) <- newRW m
                     swrite 0 0
                     twrite 0 0
                     return $ Temps swrite sread twrite tread f sep
  where f x i = let d = x - i in d*d + gsq i
        sep i u = (u*u - i*i + gsq u - gsq i) `quot` 2*(u-i)

-- Phase 2 forward scan across a row.
scan3 :: (Applicative m, Monad m) => Int -> Temps m -> Int -> Int -> m Int
scan3 m Temps{..} !q0 !u =
  do q <- iterateWhileM qaux q0
     if q < 0
     then swrite 0 u >> return 0
     else do !w <- (1+) <$> (sep <$> sread q <*> pure u)
             if w < m
             then let !q' = q+1
                  in do swrite q' u
                        twrite q' w
                        return q'
             else return q
  where qaux !q 
          | q < 0 = return Nothing
          | otherwise = do tq <- tread q
                           sq <- sread q
                           return $ if fMetric tq sq > fMetric tq u
                                    then Just $ q - 1
                                    else Nothing

-- Phase 2 backward scan across a row.
scan4 :: Monad m => 
         Int -> (Int -> Int -> m ()) -> Temps m -> Int -> Int -> m Int
scan4 rowOffset dtwrite Temps{..} !q !u = 
  do sq <- sread q
     dtwrite (rowOffset+u) (fMetric u sq)
     tq <- tread q
     if u == tq then return (q-1) else return q

phase2 :: Int -> Int -> (Int -> Int -> Int) -> Vector Int
phase2 m n gRow = V.create $
                  do dt <- VM.new (m*n)
                     mapM_ (aux (VM.unsafeWrite dt)) [0..n-1]
                     return dt
  where aux dt y = do temps <- prepTemps m (gRow y)
                      q <- foldM (scan3 m temps) 0 [1..m-1]
                      foldM (scan4 (y*m) dt temps) q [m-1,m-2..0]

sedt :: Int -> Int -> Vector Int -> Vector Int
sedt m n p = phase2 m n gRow
  where gSq = V.map (\x->x*x) $ prepareG m n p
        gRow r = (V.slice (r*m) m gSq !)

edt :: Int -> Int -> Vector Int -> Vector Float
edt w h v = V.map aux $ sedt w h v
  where aux = sqrt . fromIntegral . min 80
