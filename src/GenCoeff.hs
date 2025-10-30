module GenCoeff where

import Data.List (groupBy,sortBy)
import Matrix(headCol,multM,inverse)

genCoeff :: Fractional a => Int -> a
genCoeff n
    | n == 0 = 1
    | n == 1 = 0
    | otherwise = (fromIntegral n - 1) / 2 * genCoeff (n - 2)

expandPoly :: Num a => [(a,Int,Int)] -> [(a,Int)] -> [(a,Int,Int)]
expandPoly xs ys = do
        (x,xp,c) <- xs
        (y,yp) <- ys
        return (x*y, xp + yp,c)

simplify polys = 
    let integrated = [(v * genCoeff p, c) | (v,p,c) <- polys]
        groupedByCoeff = groupBy (\(_,c1) (_,c2) -> c1 == c2) integrated
        ((c,p):coeffs) = [ foldl (\(c1,_) (c2,e) -> (c1+c2,e)) (0,0) likeTerms | likeTerms <- groupedByCoeff]
    in (-c,p):coeffs

hermites :: Int -> [[(Rational,Int)]]
hermites 0 = [[(1,0)]]
hermites n =
    let f = (2^n,n,0):[(1,c,c+1) | c <- reverse [0..(n-1)]]
        prev = hermites (n-1)
        coeffMatrix = map (simplify . (expandPoly f)) prev
        stripped = map (map fst) coeffMatrix
        (consts, coeffs) = headCol stripped
        solved = multM (inverse coeffs) (map (:[]) consts)
        addCoeff = [(x,p) | ((x:_),p) <- zip (reverse solved) [0..]] ++ [(2^n,n)]
    in (filter (\(v,_) -> v /= 0) addCoeff):prev

