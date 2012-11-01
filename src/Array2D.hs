{-# LANGUAGE TypeFamilies, ConstraintKinds, ScopedTypeVariables, 
             FlexibleContexts #-}
module Array2D where
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as VB
import qualified Data.Vector.Generic as G
--import qualified Data.Vector.Storable as S
import Data.AffineSpace
import PointedArray
import qualified UncheckedPointedArray as U
import VectorAux
import Extended

data Array2D v a = Array2D { arrayWidth  :: Int
                           , arrayHeight :: Int
                           , arrayData   :: v a }

amap :: (G.Vector v a, G.Vector v b) => (a -> b) -> Array2D v a -> Array2D v b
amap f (Array2D w h d) = Array2D w h $ G.map f d

rows :: G.Vector v a => Array2D v a -> [v a]
rows (Array2D w h d) = go 0
  where go r | r == h = []
             | otherwise = G.slice (r*w) w d : go (r+1)

showArray :: (G.Vector v a, Show a) => Array2D v a -> String
showArray a = concat . intersperse "\n" $ map prettyRow es
  where es :: [VB.Vector String]
        es = map (VB.map show . G.convert) (rows a)
        cellSize :: Int
        cellSize = maximum (map (VB.maximum . VB.map length) es) + 1
        prettyRow :: VB.Vector String -> String
        prettyRow = concat . intersperse "," 
                  . map (\w -> replicate (cellSize - length w) ' ' ++ w)
                  . VB.toList

-- testArray :: Array2D S.Vector Float
-- testArray = Array2D 3 4 (S.fromList [1,2,3,4,5,6,7,8,9,10,11,12])

-- |Initialize a 'PointedArray' with the cursor at the origin, (0,0).
pointedArray0 :: G.Vector v a => Array2D v a -> PointedArray a
pointedArray0 (Array2D w h d) = PointedArray (Coord 0 0 w h 0)
                                             ((d G.!) . coordi)

pointedArrayAt :: G.Vector v a => v a -> Coord -> PointedArray a
pointedArrayAt d c = PointedArray c ((d G.!) . coordi)

-- |Initialize a 'U.PointedArray' with the cursor at the origin,
-- (0,0).
hotPointedArray0 :: G.Vector v a => Array2D v a -> U.PointedArray a
hotPointedArray0 (Array2D w _ d) = U.PointedArray (StrideInt 0 w) 
                                                  ((d G.!) . strideInt)

instance Cobindable (Array2D v) where
  type Ctx (Array2D v) a = G.Vector v a
  Array2D w h d =>> f = Array2D w h . unfoldN (w*h) go $ Coord 0 0 w h 0
    where go c = (f $ pointedArrayAt d c, advance c)
          advance c = fromMaybe (moveDown c) (moveRightMaybe c)

-- In Shih, background pixels are set to 0 and object pixels are +∞.
isBackground :: (Eq a, Num a) => a -> Bool
isBackground = (== 0)

--erode :: (G.Vector v a, Num a) => Array2D v a -> Array2D v a -> Array2D v a
erode :: forall t v a.
         (Cobindable t, G.Vector v a, G.Vector v (Extended a), 
          Num a, Ord a, Ctx t a) => 
         t a -> Array2D v a -> t a
erode img kernel@(Array2D w h d) = img =>> go
  where go (PointedArray c g) = unextend 40 . G.minimum $ 
                                unf ksz (kernelGo g) (kc0, c)
        unf :: Int -> (b -> (Extended a,b)) -> b -> v (Extended a)
        unf = unfoldN
        kernelGo :: (Coord -> a) -> (Coord,Coord) -> (Extended a, (Coord,Coord))
        kernelGo getPixel (kc,c) = if isBackground (getPixel c)
                                   then (Finite $ d G.! (coordi kc), advanceWith kc c)
                                   else (PosInf, advanceWith kc c)
        advanceWith kc c = case moveRightMaybe kc of
                             Nothing -> let kc' = carriage kc
                                        in (kc', c .+^ (kc' .-. kc))
                             Just kc' -> (kc', moveRight c)
        ksz = w*h
        kc0 = Coord 0 0 w h 0