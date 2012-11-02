{-# LANGUAGE BangPatterns, ConstraintKinds, TypeFamilies, RankNTypes, 
             TemplateHaskell, ScopedTypeVariables, FlexibleInstances,
             GeneralizedNewtypeDeriving #-}

-- Following Shih
import Control.Applicative
import Data.Vector.Storable (Vector, (!))
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Codec.Picture
import Codec.Picture.Types (extractLumaPlane)
import Control.Monad.ST
import GHC.Exts -- for Constraint kind

import qualified Linear.Dim as D
import Linear.V2 (V2(..))
import Linear.V3 (V3(..))
import Foreign.Storable (Storable(..))

import VectorAux
import Coord
import PointedImage
import Decomposition
import Extended

makeGray :: DynamicImage -> Image Pixel8
makeGray (ImageY8 img) = img
makeGray (ImageRGB8 img) = extractLumaPlane img
makeGray (ImageRGBA8 img) = extractLumaPlane img
makeGray _ = error "Unsupported pixel format"

class RComonad w where
  type Ctx w a :: Constraint
  extract :: Ctx w a => w a -> a
  extend :: (Ctx w a, Ctx w b) => (w a -> b) -> w a -> w b

-- Point at an image's origin
imageOrigin :: Storable a => Image a -> PointedImage a
imageOrigin img@(Image w h _) = PointedImage (Coord 0 0 0 w h)
                                             img
                                             (getElementAtI img)

neighbors9 :: PointedImage a -> V3 (V3 a)
neighbors9 (PointedImage c _ g') = 
  V3 (V3 (g $ moveUp left) (g $ moveUp c) (g $ moveUp right))
     (V3 (g left) (g c) (g right))
     (V3 (g $ moveDown left) (g $ moveDown c) (g $ moveDown right))
  where left = moveLeft c
        right = moveRight c
        g = g' . coordi

-- What if we had a function that took three 3D vectors? Then we could
-- use the unpacked vector interface to push values through the
-- consumer!
neighbors9k :: PointedImage a -> 
               (forall v. (D.Vector v a, D.Dim v ~ $(D.nat 3)) => v (v a) -> b) ->
               b
neighbors9k (PointedImage c _ g') k = k v
  where v = V3 (V3 (g $ moveUp left) (g $ moveUp c) (g $ moveUp right))
               (V3 (g left) (g c) (g right))
               (V3 (g $ moveDown left) (g $ moveDown c) (g $ moveDown right))
        left = moveLeft c
        right = moveRight c
        g = g' . coordi

instance RComonad PointedImage where
  type Ctx PointedImage a = (V.Storable a, Pixel a)
  extract (PointedImage c _ getPixel) = getPixel (coordi c)
  extend f (PointedImage c img@(Image w h _) getPixel) =
    let v = V.generate (w*h) (\i -> f (PointedImage (i2c w h i) img getPixel))
        img' = Image w h (V.unsafeCast v)
    in PointedImage c img' (getElementAtI img')

test :: IO ()
test = do Right img <- fmap makeGray <$> readImage "DT1.png"
          writePng "wat.png" $
            getPointedImage $
            extend ((`quot` 2) . extract)
                   (imageOrigin img)


-- Saturate an integer to a single byte.
sat :: Int -> Pixel8
sat = fromIntegral . min 255

distToGray :: Float -> Pixel8
distToGray = sat . floor . (* 100)

-- Erode an image with a 3x3 kernel
erode33 :: Image Pixel8 -> Vector Float -> Image Pixel8
erode33 (Image w h p) k = Image w h $ unfoldN (w*h) aux (0,0)
  where aux (x,y) = let nxt = if x == w - 1 then (0,y+1) else (x+1,y)
                    in (distToGray . V.minimum $ erodePixel x y, nxt)
        clampX = max 0 . min (w-1)
        clampY = max 0 . min (h-1)
        erodePixel x y = iunfoldN 9 (kaux x y) (-1,-1)
        kaux x y i (u,v) = let x' = clampX (x + u)
                               y' = clampY (y + v)
                               nxt = if u == 1 then (-1,v+1) else (u+1,v)
                           in if p ! (y'*w+x') > 0
                              then (k ! i, nxt)
                              else (1/0, nxt)

-- Erode an image with a square kernel
erode :: Image Pixel8 -> Vector Float -> Image Pixel8
erode (Image w h p) k = Image w h $ unfoldN (w*h) aux (0,0)
  where aux (x,y) = let nxt = if x == w - 1 then (0,y+1) else (x+1,y)
                    in (distToGray . V.minimum $ erodePixel x y, nxt)
        clampX = max 0 . min (w-1)
        clampY = max 0 . min (h-1)
        kernelWidth = (floor::Float->Int) . sqrt . fromIntegral $ V.length k
        maxU = kernelWidth `quot` 2
        erodePixel x y = iunfoldN (V.length k) (kaux x y) (-maxU,-maxU)
        kaux x y i (u,v) = let x' = clampX (x + u)
                               y' = clampY (y + v)
                               nxt = if u == maxU then (-maxU,v+1) else (u+1,v)
                           in if p ! (y'*w+x') > 0
                              then (k ! i, nxt)
                              else (1/0, nxt)

data Neighbor = Q1 | Q2 | Q3 | Q4 | Q5 | Q6 | Q7 | Q8

-- |Two-pass Euclidean distance transform from Shih and Wu, 2004.
-- The 3x3 neighborhood of each pixel, p, is indexed as
-- > q2 q3 q4
-- > q1 p  q5
-- > q8 q7 q6
edt :: Image Pixel8 -> Image Pixel8
edt (Image w h v) = undefined

g :: Neighbor -> V2 Int
g Q1 = V2 1 0
g Q5 = V2 1 0
g Q3 = V2 0 1
g Q7 = V2 0 1
g _  = V2 1 1

stapp :: Storable a => VM.STVector s a -> Int -> (a -> b) -> ST s b
stapp v i f = f <$> VM.unsafeRead v i

-- instance (Applicative m, Num a) => Num (m a) where
--   (+) = liftA2 (+)
--   (-) = liftA2 (-)
--   (*) = liftA2 (*)
--   negate = fmap negate
--   signum = fmap signum
--   abs = fmap abs
--   fromInteger = pure . fromInteger

newtype STNum s a = STNum { stnum :: ST s a } deriving (Functor, Monad, Applicative)

instance Num a => Num (STNum s a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  signum = fmap signum
  abs = fmap abs
  fromInteger = pure . fromInteger

-- |Squared Euclidean distance transform.
sedt :: Int -> Int -> Vector (Extended Int) -> Vector (Extended Int)
sedt w ht img = V.create $ 
                do v <- VM.replicate (w*ht) PosInf
                   rxv <- VM.replicate (w*ht) (0::Int)
                   ryv <- VM.replicate (w*ht) (0::Int)
                   let rx' = stapp rxv
                       ry' = stapp ryv
                       rx i = STNum (VM.unsafeRead rxv i)
                       ry i = STNum (VM.unsafeRead ryv i)
                   let -- h:: Int -> Neighbor -> ST s Int
                       h i Q1 = stnum $ 2 * rx (i-1) + 1
                       h i Q5 = stnum $ 2 * rx (i+1) + 1
                       h i Q3 = stnum $ 2 * ry (i-w) + 1
                       h i Q7 = stnum $ 2 * ry (i+w) + 1
                       h i q  = let j = pq i q
                                in stnum $ 2 * (rx j + ry j + 1)
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
        dinc x = 2 * x + 1
                


(⊖) :: Image Pixel8 -> Vector Float -> Image Pixel8
(⊖) = erode

main :: IO ()
main = do Right img <- fmap makeGray <$> readImage "DT1.png"
          writePng "DT1a.png" $ erode33 img k33
          writePng "DT1b.png" $ erode img k55
