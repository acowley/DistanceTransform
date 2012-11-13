module Main (main) where
import Criterion.Main
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import DistanceTransform.Meijster

testRes :: Int
testRes = 64

-- A cube of zeros with a one at the center.
testData :: V.Vector Int
testData = V.create $ do v <- VM.replicate (testRes^3) 1
                         VM.write v (4*testRes^2+4*testRes+4) 0
                         return v

main = do putStr "I am sane, true or false? "
          print (edt dims testData == edtPar dims testData)
          -- defaultMain [ bench "serial" $ whnf (edt dims) testData
          --             , bench "parallel" $ whnf (edtPar dims) testData ]
  where dims = replicate 3 testRes
