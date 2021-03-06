module Util where

import Data.List
import Data.Array
import Data.Ratio
import Data.Ord
import qualified Data.Set as Set
import qualified Data.Map as Map

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

nubOrd :: Ord a => [a] -> [a]
nubOrd xs = doNub Set.empty xs
  where doNub s (x:xs) | x `Set.member` s = doNub s xs
                       | otherwise        = x : doNub (Set.insert x s) xs
        doNub _ _ = []

-- removes the first occurence of the given elemen in the list
removeFirst _ []                 = []
removeFirst x (y:ys) | x == y    = ys
                     | otherwise = y : removeFirst x ys

removeFirstSymTuple _ []                 = []
removeFirstSymTuple (x1,x2) ((y1,y2):ys)
    | (x1,x2) == (y1,y2) || (x1,x2) == (y2,y1) = ys
    | otherwise = (y1,y2) : removeFirstSymTuple (x1,x2) ys

-- return the tuple if it exists in the list, regardless of order
symTupleElem :: (Eq a) => (a, a) -> [(a,a)] -> Maybe (a, a)
symTupleElem (e0, e1) l | (e0, e1) `elem` l = Just (e0, e1)
                        | (e1, e0) `elem` l = Just (e1, e0)
                        | otherwise = Nothing

swap :: [a] -> Int -> Int -> [a]
swap l i j | i > j  = swap l j i
           | i == j = l
           | i < j  = swapped
     where (a,t) = splitAt i l
           (b,c) = splitAt (j - i - 1) $ tail t
           swapped = a ++ [l!!j] ++ b ++ [l!!i] ++ tail c

-- convert a ratio to integral, in case the denominator is 1
ratioToIntegral :: (Integral a) => Ratio a -> Maybe a
ratioToIntegral x = if denominator x == 1
                        then Just $ numerator x
                        else Nothing

-- split a list, fst fulfils the predicate
splitBy :: (a -> Bool) -> [a] -> ([a], [a])
splitBy f = foldr (\x (af,bf) -> if (f x) then (x:af, bf) else (af, x:bf)) ([],[])

-- sort a map with respect to a given list of keys. In case of a
-- multimap, the order of equal keys is arbitrary
sortByList :: Ord a => [(a,b)] -> [a] -> [(a,b)]
sortByList xs ys = map snd $ sortBy (comparing fst) [(pos (fst x), x) | x <- xs]
    where order = Map.fromList $ zip ys [1..]
          pos x = Map.findWithDefault 0 x order

-- replace values in a map
replaceInMap :: (Eq a) => [(a,a)] -> [(b,a)] -> [(b,a)]
replaceInMap _ [] = []
replaceInMap map (l:ls) = case lookup (snd l) map of
                              Just x  -> (fst l, x):replaceInMap map ls
                              Nothing -> l:replaceInMap map ls
