module Util where

import Data.List
import Data.Array
{----------------------------------------------------------------------
 -
 - Util
 -
 ---------------------------------------------------------------------}

-- for a given y = (y_1,...,y_n) and a bound m, find all vectors
-- x = (x_1,...,x_n) such that |x| = m and x_i <= y_i
boundSequences :: (Num a, Ord a, Enum a) => a -> [a] -> [[a]]
boundSequences m xs
    | sm < m    = []
    | sm == m   = [xs]
    | otherwise = go sm m xs
      where
        sm = sum xs
        go _ r []
            | r == 0 = [[]]
            | otherwise = []
        go _ r [y]
            | y < r     = []
            | otherwise = [[r]]
        go s r (y:ys) = do
            let mny | s < r+y   = r+y-s
                    | otherwise = 0
                mxy = min y r
            c <- [mny .. mxy]
            map (c:) (go (s-y) (r-c) ys)

-- number of occurences of unique elements in a list
occurences :: (Ord a) => [a] -> [(a, Int)]
occurences = map (\xs@(x:_) -> (x, length xs)) . group . reverse . sort

-- append a list `e` to the list at position `n` of `list`
appendsortedAtNth :: (Show a, Ord a) => Int -> [a] -> [[a]] -> [[a]]
appendsortedAtNth n e list | length list > n = x ++ [sort (y++e)] ++ xs
                           | otherwise = error "appendAtNth: could not append"
    where (x,y:xs) = splitAt n list

-- sort and group a list by the given function
-- TODO: optimize
groupSort :: Ord k => (a -> k) -> [a] -> [[a]]
groupSort f x = map (map snd) $ groupBy fstEq $ sortBy fstCmp $ tpl f x
    where fstCmp x y = compare (fst x) (fst y)
          fstEq x y = fst x == fst y
          tpl f = map (\y -> (f y, y))

-- replace an element in a list given a predicate
replaceIf :: (a -> Bool) -> a -> [a] -> [a]
replaceIf f n = map (\x -> if f x then n else x)

-- split the longest sublist of a list of lists and return it together
-- with the rest
-- TODO: simplify
splitLongest :: [[a]] -> ([a], [[a]])
splitLongest x = (longest, before ++ after)
    where maxlength = maximum $ map length x
          maxlIx = firstAtWith (\x -> length x == maxlength) x
          (before, longest:after) = splitAt maxlIx x

-- return the index to the first element in a list fulfilling the
-- predicate
firstAtWith:: (a -> Bool) -> [a] -> Int
firstAtWith f x = countUntil f x 0
    where countUntil f (x:xs) n | f x = n
                                | otherwise = countUntil f xs (n+1)

-- replace an element in a list with another one
replaceElem :: (Eq a) => a -> a -> [a] -> [a]
replaceElem _ _ [] = []
replaceElem old new (x:xs) | x == old = new:xs
                           | otherwise = x : replaceElem old new xs

-- get the number of common Elements at the start of two lists
commonElemsAtStart :: (Num a) => [a] -> [a] -> Int
commonElemsAtStart x y = countCommon x y 0
    where countCommon [] _ c = c
          countCommon _ [] c = c
          countCommon (x:xs) (y:ys) c | x == y = countCommon xs ys (c+1)
                                      | otherwise = c

-- check if all elements of one list are contained in another list
isSubsetOf :: Eq a => [a] -> [a] -> Bool
isSubsetOf x y = all (`elem` y) x

-- compare the first elements of a list to another list
startsWith :: (Eq a) => [a] -> [a] -> Bool
startsWith _ [] = True
startsWith [] _ = True
startsWith (x:xs) (y:ys) | x /= y    = False
                         | otherwise = startsWith xs ys
