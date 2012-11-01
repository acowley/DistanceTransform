module UncheckedPointedArray (module Coord, PointedArray(..)) where
import Coord

data PointedArray a = PointedArray StrideInt (StrideInt -> a)