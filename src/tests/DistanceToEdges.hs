module DistanceToEdges where
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Data.Word
import DistanceTransform.Euclidean
import Codec.Picture
import Criterion.Main

-- Construct a square of zeros in a field of ones.
square :: Int -> V.Vector Int
square n = V.create $ do v <- VM.replicate (n*n) n
                         mapM_ (drawEdge v) edges
                         return v
  where edges = [left,right,top,bottom]
        q = n `quot` 4
        (left,right) = unzip [ (q + row*n, 3*q + row*n) | row <- [q..q*3] ]
        (top,bottom) = unzip [ (q*n + col, 3*q*n + col) | col <- [q..q*3] ]
        drawEdge v = mapM_ (flip (VM.write v) 0)

rogue :: Int -> V.Vector Int
rogue n = V.imap aux $ square n
  where aux i x | x == 0 = n+n
                | i == n * (n `quot` 2) = 0
                | otherwise = x

normalizeGray :: V.Vector Float -> V.Vector Word8
normalizeGray xs = V.map aux xs
  where hi = V.maximum xs
        lo = V.minimum xs
        s = 255 / (hi - lo)
        aux x = floor $ (x - lo) * s

toImg :: Int -> V.Vector Float -> DynamicImage
toImg n = ImageY8 . Image n n . normalizeGray

speed :: IO ()
speed = defaultMain [ bench "Square image EDT" $ whnf edt' img ]
  where n = 256
        img = square n
        edt' = edt [n,n] :: V.Vector Int -> V.Vector Float

main :: IO ()
--main = speed
main = test

test :: IO ()
test = do savePngImage "square.png" (toImg n $ V.map fromIntegral img)
          savePngImage "edt.png" (toImg n img')
          savePngImage "rogue.png" (toImg n $ V.map fromIntegral rimg)
          savePngImage "rogue-edt.png" (toImg n rimg')
  where img = square n
        img' = edt [n,n] img :: V.Vector Float
        rimg = rogue n
        rimg' = edt [n,n] $ rimg
        n = 256