module Util where

import Data.List
{----------------------------------------------------------------------
 -
 - Util
 -
 ---------------------------------------------------------------------}

-- for a given y = (y_1,...,y_n) and a bound m, find all vectors
-- x = (x_1,...,x_n) such that |x| = m and x_i <= y_i
--
-- TODO: Extend to a -> ([a] -> Bool) -> [a] -> [[a]] to and
-- filter/order according to the given predicate
boundSequences :: (Num a, Ord a, Enum a) => a -> [a] -> [[a]]
boundSequences m x | m <= sum x = (fByM . sequence . ranges) x
                   | otherwise = [[]]
    where fByM = filter (\x -> sum x == m)
          ranges = map (\x -> [0..x])


-- append a list `e` to the list at position `n` of `list`
appendAtNth :: (Show a) => Int -> [a] -> [[a]] -> [[a]]
appendAtNth n e list | length list > n = x ++ [(y++e)] ++ xs
                     | otherwise = error $ "appendAtNth: could not append"
    where (x,y:xs) = splitAt n list

-- sort and group a list by the given function
-- TODO: optimize
groupSort :: Ord k => (a -> k) -> [a] -> [[a]]
groupSort f x = map (map snd) $ groupBy fstEq $ sortBy fstCmp $ tpl f x
    where fstCmp x y = compare (fst x) (fst y)
          fstEq x y = fst x == fst y
          tpl f x = map (\y -> (f y, y)) x

-- replace an element in a list given a predicate
replaceIf :: (a -> Bool) -> a -> [a] -> [a]
replaceIf f n = map (\x -> if f x then n else x)

-- split the longest sublist of a list of lists and return it together
-- with the rest
-- TODO: simplify
splitLongest :: [[a]] -> ([a], [[a]])
splitLongest x = (longest, before ++ after)
    where maxlength = maximum $ map (length) x
          maxlIx = firstAtWith (\x -> length x == maxlength) x
          (before, (longest:after)) = splitAt maxlIx x

-- return the index to the first element in a list fulfilling the
-- predicate
firstAtWith:: (a -> Bool) -> [a] -> Int
firstAtWith f x = countUntil f x 0
    where countUntil f (x:xs) n | f x = n
                                | otherwise = countUntil f xs (n+1)

-- replace an element in a list with another one
replaceElem :: (Eq a) => a -> a -> [a] -> [a]
replaceElem _ _ [] = []
replaceElem old new (x:xs) | x == old = (new:xs)
                           | otherwise = [x] ++ replaceElem old new xs

