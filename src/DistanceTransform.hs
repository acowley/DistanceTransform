{-# LANGUAGE BangPatterns, ConstraintKinds, KindSignatures, PolyKinds, 
             FlexibleContexts, TypeFamilies, RankNTypes, DataKinds, 
             TemplateHaskell, ScopedTypeVariables, FlexibleInstances,
             InstanceSigs #-}
-- Following Shih
import Control.Applicative
import Data.Vector.Storable (Vector, (!))
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Codec.Picture
import Codec.Picture.Types (extractLumaPlane)
import GHC.Exts -- for Constraint kind

import qualified Linear.Dim as D
import Linear.V3 (V3(..))
import Linear.V4 (V4(..))

import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)
import Data.Proxy

import NatAux
import Coord

makeGray :: DynamicImage -> Image Pixel8
makeGray (ImageY8 img) = img
makeGray (ImageRGB8 img) = extractLumaPlane img
makeGray (ImageRGBA8 img) = extractLumaPlane img
makeGray _ = error "Unsupported pixel format"

-- 3x3 Euclidean distance transform kernel in row-major order.
k33 :: Vector Float
k33 = V.fromList [ a1, a0, a1
                 , a0,  0, a0
                 , a1, a0, a1 ]
  where a0 = -1
        a1 = -sqrt 2

k55 :: Vector Float
k55 = V.fromList [ b2, b1, b0, b1, b2
                 , b1, a1, a0, a1, b1
                 , b0, a0,  0, a0, b0
                 , b1, a1, a0, a1, b1
                 , b2, b1, b0, b1, b2 ]
  where a0 = -1
        a1 = -sqrt 2
        b0 = -2
        b1 = -sqrt 5
        b2 = 2 * a1

-- This is k2 following from equation 12
k2_55 :: Vector Float
k2_55 = V.fromList [ b2, b1, b0, b1, b2
                   , b1, b0, b1, b0, b1
                   , b0, b1, b0, b1, b0
                   , b1, b0, b1, b0, b1
                   , b2, b1, b0, b1, b2 ]
  where b0 = 2
        b1 = sqrt 5
        b2 = 2 * sqrt 2

-- In Shih, object pixels have value +∞ while background pixels have
-- value 0.

-- Given these kernels with positive distances. An erosion by the
-- kernel is max{k(z)} for all z that are non-zero in the image.

-- The point here is that k55 = min(k33, k2_55), and that
-- decomposition into sums (note that min/max are here reversed from
-- the paper) gives us f ⊖ k55 = max(f ⊖ k33, f ⊖ k2_55)


-- From the initial Coq treatment of morphology
-- Definition erosion (A:t) (B:t) :=
--   fold (fun a s => if for_all (fun b => mem (b ^-^ a) A) B then add a s else s) A empty.


data PointedImage a = PointedImage Coord (Image a) (Int -> a)

getElementAtI :: V.Storable a => Image a -> Int -> a
getElementAtI (Image _ _ v) i = 
  unsafePerformIO (V.unsafeWith v $ flip peekElemOff i . castPtr)

getElementAtC :: V.Storable a => Image a -> Coord -> a
getElementAtC img c = getElementAtI img (coordi c)

-- |Grab 4 elements starting at the cursor. This is wildly unsafe in
-- that it will not respect image boundaries!
unsafeGetElementsAt4 :: Storable a => PointedImage a -> V4 a
unsafeGetElementsAt4 (PointedImage c img _) = 
  getElementAtI (unsafeCoerce img) (coordi c)

class GetElementsV (n :: D.Nat) where
  getElementsAtV :: (D.Vector v a, D.Dim v ~ n, Storable a) => PointedImage a -> v a

instance GetElementsV $(D.nat 2) where
  getElementsAtV :: forall a (v :: * -> *) . 
                    (D.Vector v a, D.Dim v ~ $(D.nat 2), Storable a) =>
                    PointedImage a -> v a
  getElementsAtV (PointedImage c (Image _ _ vec) _) = 
    unsafePerformIO $
      V.unsafeWith vec $ \ptr' ->
        let ptr = castPtr ptr'
            i = coordi c
            mk = D.construct :: D.Fun $(D.nat 2) a (v a)
        in D.runFun mk <$> peekElemOff ptr i <*> peekElemOff ptr (i+1)

instance GetElementsV $(D.nat 3) where
  getElementsAtV :: forall a (v :: * -> *) . 
                    (D.Vector v a, D.Dim v ~ $(D.nat 3), Storable a) =>
                    PointedImage a -> v a
  getElementsAtV (PointedImage c (Image _ _ vec) _) = 
    unsafePerformIO $
      V.unsafeWith vec $ \ptr' ->
        let ptr = castPtr ptr'
            i = coordi c
            mk = D.construct :: D.Fun $(D.nat 3) a (v a)
        in D.runFun mk <$> peekElemOff ptr i 
                       <*> peekElemOff ptr (i+1)
                       <*> peekElemOff ptr (i+2)

instance GetElementsV $(D.nat 4) where
  getElementsAtV :: forall a (v :: * -> *) . 
                    (D.Vector v a,D.Dim v ~ $(D.nat 4), Storable a) =>
                    PointedImage a -> v a
  getElementsAtV (PointedImage c (Image _ _ vec) _) = 
    unsafePerformIO $
      V.unsafeWith vec $ \ptr' ->
        let ptr = castPtr ptr'
            i = coordi c
            mk = D.construct :: D.Fun $(D.nat 4) a (v a)
        in D.runFun mk <$> peekElemOff ptr i 
                       <*> peekElemOff ptr (i+1)
                       <*> peekElemOff ptr (i+2)
                       <*> peekElemOff ptr (i+3)

class GetElements2D (n::D.Nat) where
  getElements2D :: (D.Vector v a, D.Vector v (v a), D.Dim v ~ n, Storable a) =>
                   PointedImage a -> v (v a)

instance GetElements2D $(D.nat 2) where
  getElements2D :: forall a (v :: * -> *).
                   (D.Vector v a, D.Vector v (v a), D.Dim v ~ $(D.nat 2), Storable a) =>
                   PointedImage a -> v (v a)
  getElements2D pimg@(PointedImage c img g) = 
    D.runFun mk (getElementsAtV pimg) (getV (moveDown c))
    where mk = D.construct :: D.Fun $(D.nat 2) (v a) (v (v a))
          getV c' = getElementsAtV (PointedImage c' img g)

instance GetElements2D $(D.nat 3) where
  getElements2D :: forall a (v :: * -> *).
                   (D.Vector v a, D.Vector v (v a), D.Dim v ~ $(D.nat 3), Storable a) =>
                   PointedImage a -> v (v a)
  getElements2D pimg@(PointedImage c img g) = 
    D.runFun mk (getElementsAtV pimg) (getV c') (getV (moveDown c'))
    where mk = D.construct :: D.Fun $(D.nat 3) (v a) (v (v a))
          getV c' = getElementsAtV (PointedImage c' img g)
          c' = moveDown c

instance GetElements2D $(D.nat 4) where
  getElements2D :: forall a (v :: * -> *).
                   (D.Vector v a, D.Vector v (v a), D.Dim v ~ $(D.nat 4), Storable a) =>
                   PointedImage a -> v (v a)
  getElements2D pimg@(PointedImage c img g) = 
    D.runFun mk (getElementsAtV pimg) (getV c') (getV c'') (getV (moveDown c''))
    where mk = D.construct :: D.Fun $(D.nat 4) (v a) (v (v a))
          getV c' = getElementsAtV (PointedImage c' img g)
          c' = moveDown c
          c'' = moveDown c'

pointedPixel :: Pixel a => PointedImage a -> a
pointedPixel (PointedImage c _ getPixel) = getPixel (coordi c)

getPointedImage :: PointedImage a -> Image a
getPointedImage (PointedImage _ img _) = img

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
neighbors9 (PointedImage c (Image _ h _) g') = 
  V3 (V3 (g $ moveUp left) (g $ moveUp c) (g $ moveUp right))
     (V3 (g left) (g c) (g right))
     (V3 (g $ moveDown left) (g $ moveDown c) (g $ moveDown right))
  where left = moveLeft c
        right = moveRight c
        g = g' . coordi

-- mapInterior :: forall v a b. 
--                (D.Vector v a, NatToInt (D.Dim v), GetElementsV (D.Dim v), Storable a) => 
--                (v (v a) -> b) -> Image a -> Image b
-- mapInterior f img@(Image w h p) = 
--   Image w h $ V.create $
--         do v <- VM.new (w*h)
--            let go x y i
--                  | x == right = go half (y+1) (i+half+half)
--                  | y >= bot = return v
--                  | otherwise = let v = getElementsAtV (mkPt i) :: v a
--                                in undefined
--            go half half (w*half+half)
--   where half = natToInt (Proxy :: Proxy (D.Dim v)) `div` 2
--         right = w - half
--         bot = h - half
--         mkPt i = PointedImage (Coord 0 0 i w h) img (getElementAtI img)

mapInterior3 :: (V3 (V3 a) -> b) -> Image a -> Image b
mapInterior3 f img@(Image w h p) = undefined

-- What if we had a function that took three 3D vectors? Then we could
-- use the unpacked vector interface to push values through the
-- consumer!
neighbors9k :: (D.Vector v a, D.Dim v ~ $(D.nat 3)) => 
               PointedImage a -> 
               (forall v. (D.Vector v a, D.Dim v ~ $(D.nat 3)) => v (v a) -> b) ->
               b
neighbors9k (PointedImage c (Image _ h _) g') k = k v
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

test = do Right img <- fmap makeGray <$> readImage "DT1.png"
          writePng "wat.png" $
            getPointedImage $
            extend ((`quot` 2) . extract)
                   (imageOrigin img)


-- Saturate an integer to a single byte.
sat :: Int -> Pixel8
sat = fromIntegral . min 255

-- Like Vector's unfoldrN, but the unfolding function should be
-- total. In practice, this means that a new seed must be generated
-- when producing the last required value of the Vector. This seed
-- will never be used, but some value must be supplied.
unfoldN :: VM.Storable a => Int -> (b -> (a,b)) -> b -> Vector a
unfoldN n f z = V.create $ do v <- VM.new n
                              let go !i !s
                                    | i == n = return v
                                    | otherwise = let (x,s') = f s
                                                  in VM.unsafeWrite v i x >>
                                                     go (i+1) s'
                              go 0 z

-- Pass the function being unfolded the index of the entry as well as
-- the current seed.
iunfoldN :: VM.Storable a => Int -> (Int -> b -> (a,b)) -> b -> Vector a
iunfoldN n f z = V.create $ do v <- VM.new n
                               let go !i !s
                                     | i == n = return v
                                     | otherwise = let (x,s') = f i s
                                                   in VM.unsafeWrite v i x >>
                                                      go (i+1) s'
                               go 0 z

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
        kernelWidth = floor . sqrt . fromIntegral $ V.length k
        maxU = kernelWidth `quot` 2
        erodePixel x y = iunfoldN (V.length k) (kaux x y) (-maxU,-maxU)
        kaux x y i (u,v) = let x' = clampX (x + u)
                               y' = clampY (y + v)
                               nxt = if u == maxU then (-maxU,v+1) else (u+1,v)
                           in if p ! (y'*w+x') > 0
                              then (k ! i, nxt)
                              else (1/0, nxt)


(⊖) = erode

main = do Right img <- fmap makeGray <$> readImage "DT1.png"
          writePng "DT1a.png" $ erode33 img k33
          writePng "DT1b.png" $ erode img k55
