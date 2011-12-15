module Momentum where

import Data.List

-- representation of a vector of momenta
data Momentum e b = M [(b, e)] deriving (Eq, Ord)

-- pretty printing
instance (Num e, Show b) => Show (Momentum e b) where
    show (M []) = "0"
    show (M ts) = concatAdd $ map termToString ts
        where termToString (b,e)
                  | show b == "1" = show e
                  | show e == "1" = show b
                  | show e == "-1" = "-" ++ show b
                  | otherwise = (if isSingleExpr (show e)
                                     then show e
                                     else "(" ++ show e ++ ")") ++ "*"  ++ show b
              concatAdd [t] = t
              concatAdd (t1:t2:ts) = if head t2 == '-'
                                        then t1 ++ next
                                        else t1 ++ "+" ++ next
                        where next = concatAdd (t2:ts)

              isSingleExpr (c:cs) = isSingleExpr' cs
              isSingleExpr' [] = True
              isSingleExpr' (c:cs) | c == '+' || c == '-' = False
                                   | otherwise = isSingleExpr' cs

-- momentum labeling -> p1,p2,... for external, q1,q2,... for internal
data MomentumLabel = P Int | Q Int deriving (Eq, Ord)

instance Show MomentumLabel where
    show (P i) = "p" ++ show i
    show (Q i) = "q" ++ show i

-- basis of momentumlabels
momentumBasis :: Int -> Int -> [MomentumLabel]
momentumBasis loops legs = map P [1..legs] ++ map Q [1..loops]

-- zero momentum
zero :: Momentum elem basis
zero = M []

-- momentum from list, with simplification
fromList :: (Ord b, Num e) => [(b,e)] -> Momentum e b
fromList = simplify . M

-- momentum to list
toList :: (Ord b, Num e) => Momentum e b -> [(b,e)]
toList (M ts) = ts

-- coefficient of a specific basis element
coefficient :: (Ord b, Num e) => b -> Momentum e b -> e
coefficient b v = sum [k | (b', k) <- toList vsimp, b' == b]
    where vsimp = simplify v

-- add momenta
add :: (Ord b, Num e) => Momentum e b -> Momentum e b -> Momentum e b
add (M v1) (M v2) = M $ addMomenta v1 v2
    where addMomenta ((a,x):ts) ((b,y):us) =
             case compare a b of
                 LT -> (a,x) : addMomenta ts ((b,y):us)
                 EQ -> if x + y == 0
                            then addMomenta ts us
                            else (a,x + y) : addMomenta ts us
                 GT -> (b,y) : addMomenta ((a,x):ts) us
          addMomenta [] v = v
          addMomenta v [] = v

-- subtract momenta
subtr :: (Ord b, Num e) => Momentum e b -> Momentum e b -> Momentum e b
subtr m1 m2 = add m1 (neg m2)

-- negate momentum
neg :: (Num e) => Momentum e b -> Momentum e b
neg (M ts) = M $ map (\(x,y) -> (x,-y)) ts

-- sum a list of momenta
sumMomenta :: (Ord b, Num e) => [Momentum e b] -> Momentum e b
sumMomenta = foldl (<+>) zero

-- simplify the given momentum (combine duplicates, remove elements
-- with coefficients == 0, sort with respect to basis)
simplify :: (Ord b, Num e) => Momentum e b -> Momentum e b
simplify (M ts) = M $ simp $ sortBy compareFst ts
    where simp ((b1,x1):(b2,x2):ts) = case compare b1 b2 of
                LT -> if x1 == 0
                          then simp ((b2,x2):ts)
                          else (b1,x1):simp ((b2,x2):ts)
                EQ -> if x1 + x2 == 0
                          then simp ts
                          else simp ((b1,x1+x2):ts)
                GT -> error "simp: not presorted"

          simp [(b,0)] = []
          simp [(b,x)] = [(b,x)]
          simp [] = []
          compareFst (b1,x1) (b2,x2) = compare b1 b2

-- operators
infixl 6 <+>
infixl 6 <->

(<+>) :: (Ord b, Num e) => Momentum e b -> Momentum e b -> Momentum e b
(<+>) = add

(<->) :: (Ord b, Num e) => Momentum e b -> Momentum e b -> Momentum e b
(<->) = subtr

