{-# LANGUAGE BangPatterns, RecordWildCards #-}
module DistanceTransform.Meijster2D (edt) where
import Control.Applicative
import Control.Monad (when, foldM)
import Control.Monad.ST (ST)
import Data.Vector.Storable (Vector, (!))
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import DistanceTransform.Internal.Extended

import Debug.Trace

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

-- Build Meijster's G function that finds nearest neighbors down and
-- up columns.
prepareG m n p = V.create $ 
                 do v <- VM.new (m*n)
                    let vwrite = VM.unsafeWrite v
                        vread = VM.unsafeRead v
                        pullDown i = if p ! i == 0
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
prepTemps :: Int -> (Int -> Int) -> ST s (Temps (ST s))
prepTemps m gsq = do s <- VM.new m
                     t <- VM.new m
                     VM.unsafeWrite s 0 0
                     VM.unsafeWrite t 0 0
                     return $ Temps (VM.unsafeWrite s) (VM.unsafeRead s)
                                    (VM.unsafeWrite t) (VM.unsafeRead t)
                                    f sep
  where f x i = let d = x - i in d*d + gsq i
        sep i u = (u*u - i*i + gsq u - gsq i) `quot` 2*(u-i)

traceMsg :: Show a => String -> a -> a
traceMsg msg x = trace (msg++": "++show x) x

-- Phase 2 forward scan across a row.
scan3 :: Monad m => Int -> Temps m -> Int -> Int -> m Int
scan3 m Temps{..} !q0 !u =
  do q <- iterateWhileM qaux q0
     if q < 0
     then swrite 0 u >> return 0
     else do sq <- sread q
             let w = 1 + sep sq u
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
                  do --dt <- VM.new (m*n)
                     dt <- VM.replicate (m*n) 0
                     mapM_ (aux (VM.unsafeWrite dt)) [0..n-1]
                     return dt
  where aux dt y = do temps <- prepTemps m (gRow y)
                      q <- foldM (scan3 m temps) 0 [1..m-1]
                      foldM (scan4 (y*m) dt temps) q [m-1,m-2..0]
                      return ()
        
sedt :: (Ord a, Num a, VM.Storable a) => 
        Int -> Int -> Vector a -> Vector Int
sedt m n p = phase2 m n gRow
  where gSq = V.map (\x->x*x) $ prepareG m n p
        gRow r = (V.slice (r*m) m gSq !)

sedtBusted :: (Ord a, Num a, VM.Storable a) => 
        Int -> Int -> Vector a -> Vector Int
sedtBusted m n p = f
  where gRow :: Int -> Int -> Int
        gRow r = let g' = V.map ((\x -> x*x) . unextend (n+m)) g
                 in (V.slice (r*m) m g' !)
        g,gt :: Vector (Extended Int)
        gt = V.create $ do v <- VM.new (m*n)
                           VM.set (VM.slice 0 m v) PosInf
                           let go !i
                                 | i >= m*n = return v
                                 | p ! i == 0 = VM.unsafeWrite v i 0 >> go (i+1)
                                 | otherwise = VM.unsafeRead v (i-m) >>=
                                               VM.unsafeWrite v i . (1+) >>
                                               go (i+1)
                           go m
        g = V.create $ do v <- VM.new (m*n)
                          V.unsafeThaw (V.slice (m*n-m) m gt) >>=
                            VM.unsafeCopy (VM.slice (m*n-m) m v)
                          let go !i
                                | i < 0 = return v
                                | otherwise = (1+) <$> VM.unsafeRead v (i+m) >>=
                                              VM.unsafeWrite v i . min (gt ! i)
                                              >> go (i-1)
                          go (m*n-m-1)
        f = V.create $ 
            do s <- VM.new m
               t <- VM.new m
               dt <- VM.new (m*n)
               let rowInit = VM.unsafeWrite s 0 0 >> VM.unsafeWrite t 0 0
               let v !! i = VM.unsafeRead v i
                   phase2 !y
                     | y == n = return dt
                     | otherwise = 
                       let g' = gRow y
                           f x i = if i >= m
                                   then error ("koni "++show i)
                                   else let d = x - i in g' i + d*d
                           ff q x i = if i >= m
                                      then error ("biden "++show i ++"; q = "++show q++"; x = "++show x)
                                      else let d = x - i in g' i + d*d
                           sep i u = let gu = g' u
                                         gi = g' i
                                     in u*u-i*i+gu*gu-gi*gi `quot` 2*(u-i)
                           scan3 !u !q
                             | u == m = return q
                             | otherwise = 
                               let gu = g' u
                                   qLoop q'
                                     | q' < 0 = return q'
                                     | otherwise = do tq <- t !! q'
                                                      sq <- s !! q'
                                                      if ff q' tq sq > let d = tq - u
                                                                       in gu + d*d
                                                        then qLoop (q' - 1)
                                                        else return q'
                               in do q' <- qLoop q
                                     if q' < 0 
                                       then do VM.unsafeWrite s 0 u
                                               scan3 (u+1) 0
                                       else do sq <- s !! q'
                                               let w = 1 + sep sq u
                                               when (w < m)
                                                    (VM.unsafeWrite s q' u >>
                                                     VM.unsafeWrite t q' w)
                                               scan3 (u+1) (q'+1)
                           scan4 !u !q
                             | u < 0 = return ()
                             | otherwise = do f <$> pure u <*> (s !! q) >>=
                                                VM.unsafeWrite dt (y*m+u) 
                                              tq <- t !! q
                                              if tq == u then scan4 (u-1) (q-1)
                                                         else scan4 (u-1) q
                       in rowInit >> scan3 1 0 >>= scan4 (m-1) >> phase2 (y+1)
               phase2 0

edt :: Int -> Int -> Vector Int -> Vector Float
edt w h v = V.map aux $ sedt w h v
  where aux = sqrt . fromIntegral . min 80