module Coord where
import Data.Maybe (fromMaybe)

-- |Index into a 2D structure that can be moved with bounds checking.
data Coord = Coord { coordX      :: !Int
                   , coordY      :: !Int
                   , coordIndex  :: !Int
                   , coordWidth  :: !Int
                   , coordHeight :: !Int }

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

carriage :: Coord -> Coord
carriage (Coord _ y _ w h) = Coord 0 y' (y'*w) w h
  where y' = y + 1
