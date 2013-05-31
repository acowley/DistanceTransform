-- |Helpers for performing nested loop iteration. Includes variants
-- for parallel computation.
module DistanceTransform.Internal.Indexer where
import Control.Monad (foldM_)
import Control.Concurrent (forkIO, getNumCapabilities, 
                           newEmptyMVar, putMVar, takeMVar)
import Data.Maybe (fromMaybe)

-- | We use a zipper on list to walk over dimensions of an array.
data Zipper a = Zip [a] a [a]

-- | Create a 'Zipper' from a focal element, and a list of rightward
-- siblings.
toZipper :: a -> [a] -> Zipper a
toZipper = Zip []

-- | Create a 'Zipper' from a non-empty list, with the cursor on the
-- leftmost element. An exception is thrown if the given list is
-- empty.
unsafeToZipper :: [a] -> Zipper a
unsafeToZipper [] = error "A comonad can't be empty!"
unsafeToZipper (x:xs) = Zip [] x xs

-- | Convert a 'Zipper' to a list.
fromZipper :: Zipper a -> [a]
fromZipper (Zip l x r) = reverse l ++ x : r

-- | Move a 'Zipper' to the left.
left :: Zipper a -> Maybe (Zipper a)
left (Zip [] _ _) = Nothing
left (Zip (l:ls) x r) = Just $ Zip ls l (x:r)

unsafeLeft :: Zipper a -> Zipper a
unsafeLeft z = fromMaybe z $ left z

right :: Zipper a -> Maybe (Zipper a)
right (Zip _ _ []) = Nothing
right (Zip ls x (r:rs)) = Just $ Zip (x:ls) r rs

-- | Comonadic coreturn: produce the value a 'Zipper' is currently
-- focused upon.
focus :: Zipper a -> a
focus (Zip _ x _) = x

-- | Slide a 'Zipper' over until focused on its rightmost element.
rightmost :: Zipper a -> Zipper a
rightmost z@(Zip _ _ []) = z
rightmost (Zip ls x (r:rs)) = rightmost $ Zip (x:ls) r rs

zipSum, zipStride, zipStep :: Num a => Zipper a -> a
-- | Since we are using 'Zipper's to track the size of
-- multidemensional arrays, the sum of all zipper elements gives the
-- size of the entire array.
zipSum = sum . fromZipper

-- | Computes the stride between rows at the currently focused
-- dimension. This involves stepping over the rest of the current row
-- along all nested dimensions.
zipStride (Zip _ x rs) = product $ x:rs

-- | Computes the step between consective elements at the currently
-- focused dimension. This involves stepping over all nested
-- dimensions.
zipStep (Zip _ _ rs) = product rs

-- Each inner loop is stateful.
zipFoldM :: Monad m => Zipper Int -> (a -> Int -> m a) -> a -> [Int] -> m ()
zipFoldM (Zip ls x rs) f z indices = gol 0 (reverse ls)
  where innerDimStride = x * product rs
        gol offset [] = gor offset rs
        gol offset (d:ds) = mapM_ (\i -> gol (offset + i*stride) ds) [0..d-1]
          where stride = product ds * innerDimStride
        gor offset [] = foldM_ (\s i -> f s (offset + i*stride)) z indices
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
        golPar [] = case rs of
                      [] -> gor 0 []
                      r:rs' -> let stride = product rs'
                               in parChunkMapM_ (\i -> gor (i*stride) rs')
                                                [0..r-1]
        golPar (d:ds) = parChunkMapM_ (\i -> gol (i*stride) ds) [0..d-1]
          where stride = product ds * innerDimStride
        gol offset [] = gor offset rs
        gol offset (d:ds) = mapM_ (\i -> gol (offset + i*stride) ds) [0..d-1]
          where stride = product ds * innerDimStride
        gor offset [] = foldM_ (\s i -> f s (offset + i*stride)) z indices
          where stride = product rs
        gor offset (d:ds) = mapM_ (\i -> gor (offset + i*stride) ds) [0..d-1]
          where stride = product ds
{-# INLINE parZipFoldM #-}

zipMapM_ :: Monad m => Zipper Int -> (Int -> m ()) -> [Int] -> m ()
zipMapM_ z f = zipFoldM z (const f) ()
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
