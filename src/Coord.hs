{-# LANGUAGE TypeFamilies, TemplateHaskell #-}
module Coord where
import Control.Lens.Setter ((+~))
import Control.Lens.TH
import Data.Maybe (fromMaybe)
import Data.AdditiveGroup
import Data.AffineSpace
import Linear.V2 (V2(..))
import qualified Linear.Vector as Linear

-- |Index into a 2D structure that can be moved with bounds checking.
data Coord = Coord { _coordX      :: !Int
                   , _coordY      :: !Int
                   , _coordIndex  :: !Int
                   , _coordWidth  :: !Int
                   , _coordHeight :: !Int }
makeLenses ''Coord

-- |The difference between two 'Coord's.
newtype DiffCoord = DiffCoord { getDCoord :: V2 Int }

coordV :: Coord -> V2 Int
coordV c = V2 (_coordX c) (_coordY c)

instance AdditiveGroup DiffCoord where
  zeroV = DiffCoord 0
  negateV (DiffCoord d) = DiffCoord $ Linear.gnegate d
  DiffCoord d1 ^+^ DiffCoord d2 = DiffCoord $ d1 Linear.^+^ d2

instance AffineSpace Coord where
  type Diff Coord = DiffCoord
  c1 .-. c2 = DiffCoord $ coordV c1 Linear.^-^ coordV c2
  c .+^ DiffCoord (V2 x y) = (coordX +~ x) . (coordY +~ y) $ c

-- |A linear index into a 2D structure, with no opportunity for row
-- bounds checking.
data StrideInt = StrideInt { strideInt    :: !Int
                           , strideStride :: !Int }

coordi :: Coord -> Int
coordi (Coord _ _ i _ _) = i

i2c :: Int -> Int -> Int -> Coord
i2c w h i = let (y,x) = i `quotRem` w in Coord x y i w h

c2s :: Coord -> StrideInt
c2s (Coord _ _ i w _) = StrideInt i w

mayApply :: (a -> Maybe a) -> a -> a
mayApply f x = fromMaybe x (f x)

moveUp :: Coord -> Coord
moveUp = mayApply moveUpMaybe

moveUpMaybe :: Coord -> Maybe Coord
moveUpMaybe (Coord x y i w h)
  | y == 0 = Nothing
  | otherwise = Just $ Coord x (y-1) (i-w) w h

uncheckedUp :: StrideInt -> StrideInt
uncheckedUp (StrideInt i w) = StrideInt (i-w) w

moveDown :: Coord -> Coord
moveDown = mayApply moveUpMaybe

moveDownMaybe :: Coord -> Maybe Coord
moveDownMaybe (Coord x y i w h)
  | y == h - 1 = Nothing
  | otherwise = Just $ Coord x (y+1) (i+w) w h

uncheckedDown :: StrideInt -> StrideInt
uncheckedDown (StrideInt i w) = StrideInt (i+w) w

moveLeftMaybe :: Coord -> Maybe Coord
moveLeftMaybe (Coord x y i w h)
  | x == 0 = Nothing
  | otherwise = Just $ Coord (x-1) y (i-1) w h

moveLeft :: Coord -> Coord
moveLeft = mayApply moveLeftMaybe

uncheckedLeft :: StrideInt -> StrideInt
uncheckedLeft (StrideInt i w) = StrideInt (i-1) w

moveRightMaybe :: Coord -> Maybe Coord
moveRightMaybe (Coord x y i w h)
  | x == w - 1 = Nothing
  | otherwise = Just $ Coord (x+1) y (i+1) w h

moveRight :: Coord -> Coord
moveRight = mayApply moveRightMaybe

uncheckedRight :: StrideInt -> StrideInt
uncheckedRight (StrideInt i w) = StrideInt (i+1) w

-- |Carriage return: moves to the left-most column on the next row.
carriage :: Coord -> Coord
carriage (Coord _ y _ w h) = Coord 0 y' (y'*w) w h
  where y' = y + 1

