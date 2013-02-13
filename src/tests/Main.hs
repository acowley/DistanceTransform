-- | Uhit test that seeds a 3D grid with a few points, computes the
-- Euclidean distance transform of that grid, then checks a few points
-- to see if the distance transformed grid agrees with an exhaustive
-- nearest-neighbor search.
module Main (main) where
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assert)
import DistanceTransform.Euclidean

testRes :: Int
testRes = 64

-- A 3D point.
data Point = Point !Int !Int !Int deriving Show

pointToI :: Point -> Int
pointToI (Point x y z) = z * testRes * testRes + y * testRes + x

distance :: Point -> Point -> Float
distance (Point x1 y1 z1) (Point x2 y2 z2) = sqrt . fromIntegral $ 
                                             dx*dx + dy*dy + dz*dz
  where dx = x2 - x1
        dy = y2 - y1
        dz = z2 - z1

mkGrid :: [Point] -> V.Vector Int
mkGrid pts = V.create $ do v <- VM.replicate (testRes^(3::Int)) 1
                           mapM_ (flip (VM.write v) 0 . pointToI) pts
                           return v

main :: IO ()
main = defaultMain $ map (testPoint g1) probes ++ map (testPoint g2) probes
  where probes = [ Point 48 32 32
                 , Point 32 54 35
                 , Point 0 62 54
                 , Point 35 35 35 ]
        pts = Point mid mid mid : 
              [Point x y z | x <- [0,hi], y <- [0,hi], z <- [0,hi]]
        mid = testRes `quot` 2
        hi = testRes - 1
        rawGrid = mkGrid pts
        g1 = edt (replicate 3 testRes) rawGrid
        g2 = edtPar (replicate 3 testRes) $ rawGrid
        testPoint g probe = let x = minimum $ map (distance probe) pts
                                y = g V.! pointToI probe
                            in testCase ("Probing "++show probe)
                                        (assert (abs (x - y) < 0.0001))
