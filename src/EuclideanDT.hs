{-# LANGUAGE BangPatterns #-}
-- |2D Euclidean distance transform.
module EuclideanDT (edt) where
import Codec.Picture
import Control.Applicative
import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Vector.Storable (Vector, (!))
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Data.Word (Word8)
import Foreign.Ptr (castPtr)
import Foreign.Storable
import Linear.V2
import Extended
import STNum

data Neighbor = Q1 | Q2 | Q3 | Q4 | Q5 | Q6 | Q7 | Q8 deriving (Eq,Show,Enum,Ord)

instance Storable Neighbor where
  sizeOf _ = 1
  alignment _ = 1
  peek ptr = toEnum . fromIntegral <$> (peek (castPtr ptr) :: IO Word8)
  poke ptr q = poke (castPtr ptr) (fromIntegral $ fromEnum q :: Word8)

n1,n2 :: V.Vector Neighbor
n1 = V.fromList [Q1 .. Q4]
n2 = V.fromList [Q5 .. Q8]

-- |Two-pass Euclidean distance transform from Shih and Wu, 2004.
-- The 3x3 neighborhood of each pixel, p, is indexed as
-- > q2 q3 q4
-- > q1 p  q5
-- > q8 q7 q6
edt :: Image Pixel8 -> Image Pixel8
edt (Image w h v) = Image w h . V.map aux $
                    sedt w h v
  where aux :: Extended Int -> Word8
        aux = sat . floor . (sqrt :: Float -> Float) 
            . fromIntegral . (*100) . unextend 40
        sat :: Int -> Word8
        sat = fromIntegral . min 255

g :: Neighbor -> V2 Int
g Q1 = V2 1 0
g Q5 = V2 1 0
g Q3 = V2 0 1
g Q7 = V2 0 1
g _  = V2 1 1

mutate :: Storable a => VM.STVector s a -> Int -> (a -> a) -> ST s ()
mutate v i f = VM.unsafeRead v i >>= VM.unsafeWrite v i . f

data Direction = Forward | Backward deriving Eq

edtPass :: Direction ->
           V.Vector Neighbor ->
           (Int -> Neighbor -> Int) -> 
           VM.STVector s (Extended Int) -> 
           VM.STVector s Int ->
           VM.STVector s Int ->
           (Int -> Neighbor -> ST s Int) -> ST s ()
edtPass dir ns pq f rx ry h = go start
  where go !i 
          | i == stop = return ()
          | otherwise = let aux q = (+) <$> VM.unsafeRead f (pq i q)
                                        <*> (Finite <$> h i q)
                            update = 
                              do old <- VM.unsafeRead f i
                                 when (old /= 0) $
                                      do qs <- V.mapM aux ns
                                         let j = V.minIndex qs
                                             new = qs ! j
                                         when (new < old) $
                                              let V2 dx dy = g $ toEnum j
                                              in do VM.unsafeWrite f i new
                                                    mutate rx i (+ dx)
                                                    mutate ry i (+ dy)
                        in update >> go (step i)
        (start,stop,step) = let n = VM.length f
                            in if dir == Forward
                               then (0,n,(+1))
                               else (n-1,-1, subtract 1)

forward, backward :: (Int -> Neighbor -> Int) -> VM.STVector s (Extended Int) ->
                     VM.STVector s Int -> VM.STVector s Int ->
                     (Int -> Neighbor -> ST s Int) -> ST s ()
forward = edtPass Forward n1
backward = edtPass Backward n2

-- |Squared Euclidean distance transform.
sedt :: (Ord a, Num a, Storable a) => 
        Int -> Int -> Vector a -> Vector (Extended Int)
sedt w ht img = V.create $ 
                do --v <- VM.replicate (w*ht) PosInf
                   v <- V.thaw (V.map (\x -> if x > 0 then PosInf else 0) img)
                   rxv <- VM.replicate (w*ht) (0::Int)
                   ryv <- VM.replicate (w*ht) (0::Int)
                   let rx i = STNum (VM.unsafeRead rxv i)
                       ry i = STNum (VM.unsafeRead ryv i)
                   let -- h:: Int -> Neighbor -> ST s Int
                       h i Q1 = stnum $ 2 * rx (i-1) + 1
                       h i Q5 = stnum $ 2 * rx (i+1) + 1
                       h i Q3 = stnum $ 2 * ry (i-w) + 1
                       h i Q7 = stnum $ 2 * ry (i+w) + 1
                       h i q  = let j = pq i q
                                in stnum $ 2 * (rx j + ry j + 1)
                   forward pq v rxv ryv h
                   backward pq v rxv ryv h 
                   return v
  where -- Compute image index of a neighbor
        pq :: Int -> Neighbor -> Int
        pq i Q1 = i - 1
        pq i Q2 = i - w - 1
        pq i Q3 = i - w
        pq i Q4 = i - w + 1
        pq i Q5 = i + 1
        pq i Q6 = i + w + 1
        pq i Q7 = i + w
        pq i Q8 = i + w - 1
