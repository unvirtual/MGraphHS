module Morphisms (refine) where

import Graph
import Util
import DFS
import Data.Array

{----------------------------------------------------------------------
 -
 - Isomorphisms and automorphisms of graphs
 -
 - Ref: Practical Graph Isomorphism - B.D.McKak 1981
 -
 ---------------------------------------------------------------------}
 
{---------------------------------------------------------------------
 -
 - Public exports
 -
 ---------------------------------------------------------------------}

-- refine a partition with respect to another partition
refine :: UGraph -> Partition -> [Cell] -> Partition
refine gr pi [] = pi
refine gr pi (w:ws) = refine gr pinew wsnew
    where (pinew, wsnew) = refinePi gr pi w ws

{---------------------------------------------------------------------
 -
 - Internal
 -
 ---------------------------------------------------------------------}

type Cell = [Vertex]
-- set of ordered disjoint non-empty cells of a set S with union P = S
-- Use mutable arrays of mutable arrays as many updates of the
-- elements are necessary
type Partition = [Cell]

-- safe partition constructor
partition :: Bounds -> [Cell] -> Partition
partition bnds c | concat c == [fst bnds..snd bnds] = c
                 | otherwise = error $ "parition: given cells don't form a union on"
                               ++ show bnds

unitPartition :: Bounds -> Partition
unitPartition bounds = [range bounds]

-- a cell is trivial if it has cardinality = 1
isTrivial :: Cell -> Bool
isTrivial = (==) 1 . length

-- a partition is discrete if all cells are trivial (cardinality = 1)
isDiscrete :: Partition -> Bool
isDiscrete = all (isTrivial)

isUnit :: Partition -> Bool
isUnit = (==) 1 . length

fix :: Partition -> [Vertex]
fix = concat . filter isTrivial

supp :: Partition -> [Vertex]
supp = concat . filter (not . isTrivial)

-- refine a given current pi elem Pi_G recursively given a w and ws,
-- return the new pi and the modified ws
refinePi :: UGraph -> Partition -> Cell -> [Cell] -> (Partition, [Cell])
refinePi gr [] wi ws = ([], ws)
refinePi gr pi@(pie:pis) wi ws
    | isDiscrete pi = (pi, ws)
    | otherwise = (finalpi, finalws)
    where finalpi = xx ++ newp
          (xx, newws) = refinePiE gr pie wi ws
          (newp, finalws) = refinePi gr pis wi newws

-- Given a cell pie in pi, a list of cells ws and an element w, return
-- the ordered parition of pie with respect to w and the new list of
-- cells ws
refinePiE :: UGraph -> Cell -> Cell -> [Cell] -> (Partition, [Cell])
refinePiE gr pie wi ws
    | isUnit xx = ([pie], ws)
    | otherwise = (xx, wsmod)
    where xx = orderedPartition gr wi pie
          (maxX, restX) = splitLongest xx
          wsmod = (replaceElem pie maxX ws) ++ restX

-- Given a cell W find X = (X_1,..,X_s) which is an element of all partition on a
-- given cell V such that d(x,W) < d(y,W) for x in X_i and y in X_j if i<j
orderedPartition :: UGraph -> Cell -> Cell -> Partition
orderedPartition gr w v = groupSort (\x -> cellDegree gr w x) v
    where -- number of vertices in a cell adjacent to a given vertex
          cellDegree :: UGraph -> Cell -> Vertex -> Int
          cellDegree gr c v = sum $ map fromEnum $ map (isNeighbour gr v) c

