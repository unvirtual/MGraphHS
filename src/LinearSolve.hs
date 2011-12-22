{-# LANGUAGE TypeFamilies #-}
module LinearSolve (linearSolve) where

import Util
import Algebra
import Debug.Trace

{----------------------------------------------------------------------
 -
 - Export
 -
 ----------------------------------------------------------------------}

-- solve an inhomogeneous system M(i,j) x(j) = b(i) for \vec x
linearSolve :: (Module a, b ~ Scalar a, Fractional b) => (Matrix b, Row a) -> Row a
linearSolve x = solveTriangular $ finalRPE
                                $ foldl reduceRow x [0..length (fst x) - 2]
    where finalRPE (m,b) = (init m ++ [init $ last m ++ [1]],
                            init b ++ [last b </ (last $ last m)])

{----------------------------------------------------------------------
 -
 - Internal
 -
 ----------------------------------------------------------------------}

type Matrix a = [[a]]
type Row a = [a]

solveTriangular :: (Module a, b ~ Scalar a) => (Matrix b, Row a) -> Row a
solveTriangular x@(mat,b) = fst $ foldr next ([last b], init b) (init mat)
    where next m (bv, b) = let sp = drop (length mat - length bv) m
                               sol = last b .-. (Algebra.sum $ zipWith (<*) bv sp)
                           in (sol:bv, init b)

swapRows :: (Matrix b, Row a) -> Int -> Int -> (Matrix b, Row a)
swapRows x@(m,r) i j = (swap m i j, swap r i j)

reduceRow :: (Module a, b ~ Scalar a, Fractional b) => (Matrix b, Row a) -> Int -> (Matrix b, Row a)
reduceRow x@(m,b) r = (take r swMat ++ [normrow] ++ matRest,
                       take r swB ++ [firstB] ++ bRest)
    where nonvIndex = head $ filter (\x -> m!!x!!r /= 0) [r..length m - 1]
          (swMat, swB) = swapRows x r nonvIndex

          row = swMat !! r
          firstB = (swB!!r) </ (row!!r)
          normrow = map (\x -> x / (row!!r)) row
          subtractInRow nrow i r = zipWith (\x y -> (r!!i)*x - y) nrow r
          subtractInB c n = zipWith (\x y -> x *> n .-. y) c
          col = map (\x -> swMat!!x!!r) [r..length m - 1]

          matRest = map (subtractInRow normrow r) $ drop (r+1) swMat
          bRest   = subtractInB (tail col) firstB $ drop (r+1) swB
