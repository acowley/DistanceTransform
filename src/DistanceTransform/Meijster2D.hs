{-# LANGUAGE BangPatterns #-}
module DistanceTransform.Meijster2D (edt) where
import Control.Applicative
import Control.Monad (when)
import Data.Vector.Storable (Vector, (!))
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import DistanceTransform.Internal.Extended

data Metric = EDT | MDT | CDT

-- rows :: VM.Storable a => Int -> Vector a -> (Int -> Int -> a)
-- rows stride v r i = v ! (r*stride+1)

-- cols :: VM.Storable a => Int -> Vector a -> (Int -> Int -> a)
-- cols stride v c i = v ! (i*stride+c)

-- Suppose we represent a column (or a row) as a function Int ->
-- a. Then we can offer an interface to apply functions to rows or
-- columns. Such a function would have type ((Int -> a) -> m ()). But
-- this just provides read only access to an array! What we really
-- care about here is the cursor. To make matters worse, we also want
-- to be able to restrict the range of values we apply our function
-- to.

-- Function @f@ will be applied sequentially along each row.
rowsM :: Monad m => Int -> Int -> [Int] -> (Int -> m ()) -> m ()
rowsM m n cs f = mapM_ aux [0..n-1]
  where aux r = mapM_ f $ map (r*m +) cs -- [r*m..r*m+m-1]

-- Function @f@ will be applied sequentially down each column.
colsM :: Monad m => Int -> Int -> [Int] -> (Int -> m ()) -> m ()
colsM m n rs f = mapM_ aux [0..m-1]
  where aux c = mapM_ f $ map ((c +) . (m*)) rs -- [c,c+m .. m*(n-1)+c]

-- But I want to be able to go backwards and forwards!

-- rowsM :: (Monad m, VM.Storable a) => 
--          Int -> Int -> Vector a -> (Int -> m ()) -> m ()
-- rowsM m n f = mapM_ aux [0..n-1]
--   where aux r = let offset = r * m
--                 in f -> p ! (offset+c))

-- colsM :: (Monad m, VM.Storable a) =>
--          Int -> Int -> Vector a -> ((Int -> a) -> m ()) -> m ()
-- colsM m n p f = mapM_ aux [0..m-1]
--   where aux c = f (\r -> p ! (r*m+c))

intervalApply :: Monad m => 
                 Int -> (Int -> Bool) -> (Int -> Int) -> (Int -> m ()) -> m ()
intervalApply start stop step f = go start
  where go !i | stop i = return ()
              | otherwise = f i >> go (step i)

fillGen :: Monad m => Int -> Int -> (Int -> Int) -> (Int -> m ()) -> m ()
fillGen start n step f = go 0
  where go !i | i == n = return ()
              | otherwise = f i >> go (step i)

-- Build Meijster's G function that finds nearest neighbors down and
-- up columns.
prepareG m n p = V.create $ 
                 do v <- VM.new (m*n)
                    VM.set (VM.slice 0 m v) (m+n) -- m+n ~ PosInf
                    let vwrite = VM.unsafeWrite v
                        vread = VM.unsafeRead v
                        pullDown i = if p ! i == 0
                                     then vwrite i 0
                                     else vread (i-m) >>= vwrite i . (1+)
                        pushUp i = do d <- min <$> ((+1) <$> vread i)
                                               <*> vread (i+m)
                                      vwrite i d
                    colsM m n [1..n-1] pullDown
                    colsM m n [n-2..0] pushUp
                    --fillGen m (m*n-m) (+1) pullDown
                    return v

sedt :: (Ord a, Num a, VM.Storable a) => 
        Int -> Int -> Vector a -> Vector Int
sedt m n p = undefined
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