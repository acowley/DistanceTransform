module DistanceTransform.Indexer where
import Control.Monad (foldM)
import Data.List (foldl')
import Control.Concurrent (forkIO, getNumCapabilities, 
                           newEmptyMVar, putMVar, takeMVar)

-- We use a zipper on list to walk over dimensions.
data Zipper a = Zip [a] a [a]

toZipper :: a -> [a] -> Zipper a
toZipper = Zip []

unsafeToZipper :: [a] -> Zipper a
unsafeToZipper [] = error "A comonad can't be empty!"
unsafeToZipper (x:xs) = Zip [] x xs

fromZipper :: Zipper a -> [a]
fromZipper (Zip l x r) = reverse l ++ x : r

left :: Zipper a -> Maybe (Zipper a)
left (Zip [] _ _) = Nothing
left (Zip (l:ls) x r) = Just $ Zip ls l (x:r)

unsafeLeft :: Zipper a -> Zipper a
unsafeLeft z = maybe z id $ left z

right :: Zipper a -> Maybe (Zipper a)
right (Zip _ _ []) = Nothing
right (Zip ls x (r:rs)) = Just $ Zip (x:ls) r rs

focus :: Zipper a -> a
focus (Zip _ x _) = x

rightmost :: Zipper a -> Zipper a
rightmost z@(Zip _ _ []) = z
rightmost (Zip ls x (r:rs)) = rightmost $ Zip (x:ls) r rs

zipSum,zipProd,zipStride,zipStep :: Num a => Zipper a -> a
zipSum = sum . fromZipper
zipProd = product . fromZipper
zipStride (Zip _ x rs) = product $ x:rs
zipStep (Zip _ _ rs) = product rs

zipFoldl :: Zipper b -> (a -> b -> a) -> a -> a
zipFoldl (Zip _ x rs) f z = foldl f z (x:rs)

zipFoldl' :: Zipper b -> (a -> b -> a) -> a -> a
zipFoldl' (Zip _ x rs) f z = foldl' f z (x:rs)

zipFoldr :: Zipper b -> (b -> a -> a) -> a -> a
zipFoldr (Zip ls x rs) f z = foldr f z (x:ls)

{-
-- We have a multidimensional data structure packed into a flat array,
-- and we want to iterate over a particular dimension in an inner loop
-- with an outer loop for each other dimension.
zipMapM_ :: Monad m => Zipper Int -> (Int -> m ()) -> [Int] -> m ()
zipMapM_ (Zip ls x rs) f indices = gol 0 (reverse ls)
  where innerDimStride = x * product rs
        gol offset [] = gor offset rs
        gol offset (d:ds) = mapM_ (\i -> gol (offset + i*stride) ds) [0..d-1]
          where stride = product ds * innerDimStride
        gor offset [] = mapM_ (\i -> f (offset + i * stride)) indices
          where stride = product rs
        gor offset (d:ds) = mapM_ (\i -> gor (offset + i*stride) ds) [0..d-1]
          where stride = product ds
-}

-- Each inner loop is stateful.
zipFoldM :: Monad m => Zipper Int -> (a -> Int -> m a) -> a -> [Int] -> m ()
zipFoldM (Zip ls x rs) f z indices = gol 0 (reverse ls)
  where innerDimStride = x * product rs
        gol offset [] = gor offset rs
        gol offset (d:ds) = mapM_ (\i -> gol (offset + i*stride) ds) [0..d-1]
          where stride = product ds * innerDimStride
        gor offset [] = foldM (\s i -> f s (offset + i*stride)) z indices >>
                        return ()
          where stride = product rs
        gor offset (d:ds) = mapM_ (\i -> gor (offset + i*stride) ds) [0..d-1]
          where stride = product ds
{-# INLINE zipFoldM #-}

parChunkMapM_ :: (a -> IO ()) -> [a] -> IO ()
parChunkMapM_ f xs0 = do caps <- getNumCapabilities
                         let sz = length xs0 `quot` caps
                         let chunk ts [] = sequence_ ts
                             chunk ts xs = let (c,xs') = splitAt sz xs
                                           in do m <- newEmptyMVar
                                                 _ <- forkIO $ mapM_ f c >> 
                                                               putMVar m ()
                                                 chunk (takeMVar m:ts) xs'
                         chunk [] xs0

parZipFoldM :: Zipper Int -> (a -> Int -> IO a) -> a -> [Int] -> IO ()
parZipFoldM (Zip ls x rs) f z indices = golPar $ reverse ls
  where innerDimStride = x * product rs
        golPar [] = parChunkMapM_ (\i -> gor (i*stride) rs') [0..r-1]
          where r:rs' = rs
                stride = product rs'
        golPar (d:ds) = parChunkMapM_ (\i -> gol (i*stride) ds) [0..d-1]
          where stride = product ds * innerDimStride
        gol offset [] = gor offset rs
        gol offset (d:ds) = mapM_ (\i -> gol (offset + i*stride) ds) [0..d-1]
          where stride = product ds * innerDimStride
        gor offset [] = foldM (\s i -> f s (offset + i*stride)) z indices >>
                        return ()
          where stride = product rs
        gor offset (d:ds) = mapM_ (\i -> gor (offset + i*stride) ds) [0..d-1]
          where stride = product ds
{-# INLINE parZipFoldM #-}

zipMapM_ :: Monad m => Zipper Int -> (Int -> m ()) -> [Int] -> m ()
zipMapM_ z f is = zipFoldM z (const f) () is
{-# INLINE zipMapM_ #-}

-- Give a function an offset to the start of its indices and the step
-- between indices. This lets you walk along *any* dimension within a
-- packed array. The idea is to let the caller do whatever the heck
-- they want in that traversal.
zipFoldMAsYouDo :: Monad m => Zipper Int -> (Int -> Int -> m ()) -> m ()
zipFoldMAsYouDo z f = zipFoldM z auxOffset Nothing [0,1]
  where auxOffset Nothing offset = return $ Just offset
        auxOffset (Just offset) step' = f offset (step' - offset) >>
                                        return Nothing
{-# INLINE zipFoldMAsYouDo #-}

-- Give a function an offset to the start of its indices and the step
-- between indices. This lets you walk along *any* dimension within a
-- packed array. The idea is to let the caller do whatever the heck
-- they want in that traversal.
parZipFoldMAsYouDo :: Zipper Int -> (Int -> Int -> IO ()) -> IO ()
parZipFoldMAsYouDo z f = parZipFoldM z auxOffset Nothing [0,1]
  where auxOffset Nothing offset = return $ Just offset
        auxOffset (Just offset) step' = f offset (step' - offset) >>
                                        return Nothing
{-# INLINE parZipFoldMAsYouDo #-}


test2 = zipFoldMAsYouDo (unsafeLeft . rightmost $ unsafeToZipper [3,3,3]) aux
  where aux offset step = putStrLn $ "Row from "++show offset++" by "++show step

test = zipMapM_ cursor print [0..3-1]
  where cursor = unsafeLeft . unsafeLeft . rightmost $ unsafeToZipper [3,3,3]

{-
0 1 2
3 4 5
6 7 8

 9 10 11
12 13 14
15 16 17

18 19 20
21 22 23
24 25 26
-}

{- So if I want to iterate over the second dimension, I expect to get:
0 3 6 1 4 7 2 5 8
9 12 15 10 13 16 11 14 17 ...
-}
