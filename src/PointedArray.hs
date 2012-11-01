{-# LANGUAGE ConstraintKinds, TypeFamilies #-}
module PointedArray (module Coord, PointedArray(..), Cobindable(..)) where
import Coord
import GHC.Exts (Constraint)

data PointedArray a = PointedArray Coord (Coord -> a)

-- Comonadic cobind
class Cobindable w where
  type Ctx w a :: Constraint
  (=>>) :: (Ctx w a, Ctx w b) => w a -> (PointedArray a -> b) -> w b
