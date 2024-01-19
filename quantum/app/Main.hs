module Main (main) where

import qualified Numeric.LinearAlgebra as N
import Graphics.Rendering.Chart
import qualified Math.NumberTheory.Roots as M

mat :: N.Matrix N.Z
mat = (3 N.><4)[0..]

x :: N.Matrix N.C
x = (2 N.><2)[0,1,1,0]

z :: N.Matrix N.C
z = (2 N.><2)[1,0,0,-1]

y :: N.Matrix N.C
y = (2 N.><2)[0,0 N.:+(-1),0 N.:+(1),0]

qmc :: N.Matrix N.C
qmc = sum [x `N.kronecker` x, y `N.kronecker` y, z `N.kronecker` z]

-- a matrix created by acting matrix A on qubit
mNAM :: (Num a, N.Product a, N.Element a) => Int -> N.Matrix a -> Int -> N.Matrix a
mNAM n a m = N.ident (2^n) `N.kronecker` a `N.kronecker` N.ident (2^m)

vec :: N.Element t => N.Matrix t -> N.Vector t
vec = N.flatten

vecLength :: N.Element t => N.Vector t -> Int
vecLength = length . N.toList

unvec :: N.Element t => N.Vector t -> Maybe (N.Matrix t)
unvec v 
    | M.isSquare . vecLength $ v = Just (((M.integerSquareRoot . vecLength $ v) N.>< (M.integerSquareRoot . vecLength $ v)) (N.toList v))
    | otherwise                 = Nothing

main :: IO ()
main = putStrLn . show $ mat


