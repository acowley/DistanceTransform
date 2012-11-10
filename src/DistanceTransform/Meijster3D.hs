module DistanceTransform.Meijster3D (edt) where
import Data.Vector.Storable (Vector, (!))
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import DistanceTransform.Internal.Extended

sedt :: (Ord a, Num a, VM.Storable a) => 
        Int -> Vector a -> Vector (Extended Int)
sedt = undefined

edt :: Int -> Vector Int -> Vector Float
edt stride v = V.map aux $ sedt stride v
  where aux = sqrt . fromIntegral . min 80 . unextend 40