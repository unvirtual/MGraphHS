module Morphisms (refine) where

import Graph
import Util
import DFS
import Data.Array
import Data.Tree
import Data.Int
import Data.Bits

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

{----------------------------------------------------------------------
 -
 - Straight-forward, potentially inefficient implementation
 -
 ----------------------------------------------------------------------}

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

-- number of vertices in a cell adjacent to a given vertex
cellDegree :: UGraph -> Cell -> Vertex -> Int
cellDegree gr c v = sum $ map fromEnum $ map (isNeighbour gr v) c

-- the scalar product pi.v of a partition with a vertex is defined as
-- extracting the vertex from its cell and inserting it as a trivial
-- cell before the matching cell 
parallelProject :: Partition -> Vertex -> Partition
parallelProject [] _ = []
parallelProject (p:ps) v | (not . isTrivial) p && vInCell v p = ([v]:extractV v p:ps)
                         | otherwise = [p] ++ parallelProject ps v
    where vInCell v c = v `elem` c
          extractV _ [] = []
          extractV v (c:cs) | c == v = cs
                            | otherwise = [c] ++ extractV v cs

-- the orthogonal projection pi \ortho v of a partition and a vertex
-- is defined as R(G, pi.v, [[v]])
orthoProject :: UGraph -> Partition -> Vertex -> Partition
orthoProject gr pi v = refine gr (parallelProject pi v) [[v]]

-- next level of descendants of given parition
descendantPartitions :: UGraph -> Partition -> [Partition]
descendantPartitions g p = dp g p (verts p)
    where verts (p:ps) | isTrivial p = verts ps
                       | otherwise = p
          verts [] = []
          dp g p (v:vs) = (orthoProject g p v:dp g p vs)
          dp _ _ [] = []

-- build the search tree
partitionTree :: UGraph -> Partition -> Tree (Partition, Int32)
partitionTree g p = buildTree (refine g p p)
    where buildTree p = Node (p, indicator g p) (map (buildTree) (descendantPartitions g p))

-- trivial indicator function (hash for given partition)
indicator :: UGraph -> Partition -> Int32
indicator g = orderedHash . map unorderedHash . inner
    where inner [] = []
          inner (p:ps) = (map (fromIntegral . cellDegree g p) (vertices g) : inner ps)

-- orer independent hash of a vector
unorderedHash :: [Int32] -> Int32
unorderedHash = foldl (xor) 0

-- rotating hash
orderedHash :: [Int32] -> Int32
orderedHash = foldl (\acc x -> ((shift acc 7) `xor` (shift acc (-28)) `xor` x)) 0