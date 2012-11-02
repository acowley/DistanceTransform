{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, KindSignatures, 
             TypeFamilies, DataKinds, FlexibleInstances, InstanceSigs #-}
module PointedImage where
import Codec.Picture
import Control.Applicative
import qualified Data.Vector.Storable as V
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))
import Linear.Dim
import Linear.V4 (V4)
import System.IO.Unsafe
import Unsafe.Coerce (unsafeCoerce)
import Coord

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

class GetElementsV (n :: Nat) where
  getElementsAtV :: (Vector v a, Dim v ~ n, Storable a) => PointedImage a -> v a

instance GetElementsV $(nat 2) where
  getElementsAtV :: forall a (v :: * -> *) . 
                    (Vector v a, Dim v ~ $(nat 2), Storable a) =>
                    PointedImage a -> v a
  getElementsAtV (PointedImage c (Image _ _ vec) _) = 
    unsafePerformIO $
      V.unsafeWith vec $ \ptr' ->
        let ptr = castPtr ptr'
            i = coordi c
            mk = construct :: Fun $(nat 2) a (v a)
        in runFun mk <$> peekElemOff ptr i <*> peekElemOff ptr (i+1)

instance GetElementsV $(nat 3) where
  getElementsAtV :: forall a (v :: * -> *) . 
                    (Vector v a, Dim v ~ $(nat 3), Storable a) =>
                    PointedImage a -> v a
  getElementsAtV (PointedImage c (Image _ _ vec) _) = 
    unsafePerformIO $
      V.unsafeWith vec $ \ptr' ->
        let ptr = castPtr ptr'
            i = coordi c
            mk = construct :: Fun $(nat 3) a (v a)
        in runFun mk <$> peekElemOff ptr i 
                       <*> peekElemOff ptr (i+1)
                       <*> peekElemOff ptr (i+2)

instance GetElementsV $(nat 4) where
  getElementsAtV :: forall a (v :: * -> *) . 
                    (Vector v a,Dim v ~ $(nat 4), Storable a) =>
                    PointedImage a -> v a
  getElementsAtV (PointedImage c (Image _ _ vec) _) = 
    unsafePerformIO $
      V.unsafeWith vec $ \ptr' ->
        let ptr = castPtr ptr'
            i = coordi c
            mk = construct :: Fun $(nat 4) a (v a)
        in runFun mk <$> peekElemOff ptr i 
                       <*> peekElemOff ptr (i+1)
                       <*> peekElemOff ptr (i+2)
                       <*> peekElemOff ptr (i+3)

class GetElements2D (n::Nat) where
  getElements2D :: (Vector v a, Vector v (v a), Dim v ~ n, Storable a) =>
                   PointedImage a -> v (v a)

instance GetElements2D $(nat 2) where
  getElements2D :: forall a (v :: * -> *).
                   (Vector v a, Vector v (v a), Dim v ~ $(nat 2), Storable a) =>
                   PointedImage a -> v (v a)
  getElements2D pimg@(PointedImage c img g) = 
    runFun mk (getElementsAtV pimg) (getV (moveDown c))
    where mk = construct :: Fun $(nat 2) (v a) (v (v a))
          getV c' = getElementsAtV (PointedImage c' img g)

instance GetElements2D $(nat 3) where
  getElements2D :: forall a (v :: * -> *).
                   (Vector v a, Vector v (v a), Dim v ~ $(nat 3), Storable a) =>
                   PointedImage a -> v (v a)
  getElements2D pimg@(PointedImage c img g) = 
    runFun mk (getElementsAtV pimg) (getV c') (getV (moveDown c'))
    where mk = construct :: Fun $(nat 3) (v a) (v (v a))
          getV c' = getElementsAtV (PointedImage c' img g)
          c' = moveDown c

instance GetElements2D $(nat 4) where
  getElements2D :: forall a (v :: * -> *).
                   (Vector v a, Vector v (v a), Dim v ~ $(nat 4), Storable a) =>
                   PointedImage a -> v (v a)
  getElements2D pimg@(PointedImage c img g) = 
    runFun mk (getElementsAtV pimg) (getV c') (getV c'') (getV (moveDown c''))
    where mk = construct :: Fun $(nat 4) (v a) (v (v a))
          getV c' = getElementsAtV (PointedImage c' img g)
          c' = moveDown c
          c'' = moveDown c'

pointedPixel :: Pixel a => PointedImage a -> a
pointedPixel (PointedImage c _ getPixel) = getPixel (coordi c)

getPointedImage :: PointedImage a -> Image a
getPointedImage (PointedImage _ img _) = img
