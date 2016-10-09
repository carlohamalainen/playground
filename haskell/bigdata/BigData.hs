module BigData where

import Control.Monad
import Data.Complex
import Data.Packed.Matrix
import Data.List
import Numeric.Container
import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.HMatrix
import Text.Printf

import SampleData (a)

import Plots (doPlot)

buildW :: Double -> Matrix Double -> Matrix Double
buildW sigma a = buildMatrix n n $ \(i,j) -> f sigma a i j
  where
    n = rows a

    f sigma a i j = if j /= i
                        then expp sigma a i j
                        else 0.0

    dist :: Matrix Double -> Int -> Int -> Double
    dist m i j = norm2 $ fromList [ m!i!0 - m!j!0, m!i!1 - m!j!1 ]

    expp :: Double -> Matrix Double -> Int -> Int -> Double
    expp sigma m i j = exp $ (-(dist m i j)**2)/(2*sigma**2)

buildD :: Matrix Double -> Matrix Double
buildD w = diag
         . fromList
         . map (foldVector (+) 0)
         . toRows
         $ w
  where
    n = rows w

fiedler :: Double -> Matrix Double -> (Double, Vector Double)
fiedler sigma m = (val ! (n-2), vector $ concat $ toLists $ vec ¿ [n-2])
  where
    (val, vec) = lapEigs sigma m
    n = rows m

lapEigs :: Double -> Matrix Double -> (Vector Double, Matrix Double)
lapEigs sigma m = eigSH l
  where
    w = buildW sigma m
    d = buildD w
    l = d - w

-- checkId = maximum $ concat $ toLists $ l - (vec <> diag val <> tr vec)

main :: IO ()
main = do
    let sigma    = 1.0
        (val, _) = lapEigs sigma a
       
        (algConnecEigVal, algConnecEigVec) = fiedler sigma a

    doPlot "eigenvalues.png" "Eigenvalues" "eigenvalue" $ zip [0..] (reverse $ toList val)

    doPlot "fiedler.png"
           "Second eigenvalue of unnormalised Laplacian"
           "fiedler eigenvector"
           (zip [0..] $ toList algConnecEigVec)
