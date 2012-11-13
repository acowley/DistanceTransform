import Data.List (intersperse)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import DistanceTransform.Euclidean3D
import Text.Printf
import qualified DistanceTransform.Meijster2D as M2D
import qualified DistanceTransform.Meijster as Mn

testRes :: Int
testRes = 9

-- A cube of zeros with a one at the center.
testData :: V.Vector Int
testData = V.create $ do v <- VM.replicate (testRes^3) 1
                         VM.write v (4*testRes^2+4*testRes+4) 0
                         return v

showSquare :: VM.Storable a => Int -> (a -> String) -> V.Vector a -> IO ()
showSquare res shw v = do putStr "["
                          putStrLn (head rows')
                          mapM_ (putStrLn . (" "++)) (take (res - 2) $ tail rows')
                          putStr (" "++last rows')
                          putStrLn "]"
  where rows = map showRow [0..res-1]
        rows' = map (concat . intersperse " " . map pad) rows
        showRow r = map shw . V.toList $ 
                    V.slice (r*res) res v
        maxWord = maximum $ map (maximum . map length) rows
        pad w = let d = maxWord - length w
                in if d > 0
                   then replicate d ' ' ++ w
                   else w

showCube :: VM.Storable a => Int -> (a -> String) -> V.Vector a -> IO ()
showCube res shw v = mapM_ ((>> putStrLn "") . showPlane) [0..res-1]
  where showPlane z = showSquare res shw $ V.slice (z*res*res) (res*res) v

testDT = edt testRes testData

testDT2 = Mn.edt [testRes,testRes,testRes] testData
showDT2 = showCube testRes (printf "%0.1f") testDT2

showDT = showCube testRes (printf "%0.1f") testDT

testDT2D = showSquare testRes (printf "%0.1f") . M2D.edt testRes testRes $ 
           V.create $ do v <- VM.replicate (testRes^2) 1
                         VM.write v (4*testRes+4) 0
                         return v

testDTND = showSquare testRes (printf "%0.1f") . Mn.edt [testRes,testRes] $ 
           V.create $ do v <- VM.replicate (testRes^2) 1
                         VM.write v (4*testRes+4) 0
                         return v