module Graph ( Vertex
             , Edge
             , Bounds
             , UGraph
             , vertices
             , edges
             , vertexBounds
             , adjacency
             , adjVertices
             , degree) where

import Data.Array

{--------------------------------------------------------------------------------
 -
 - Public interface
 -
 -------------------------------------------------------------------------------}

type Vertex = Int
type Edge = (Vertex, Vertex)
type Bounds = (Vertex, Vertex)
-- undirected unlabeled graph represented by a symmetric adjacency
-- matrix
type UGraph = Array SymIx Int

-- build a graph from the given edges
createUGraph :: [Edge] -> UGraph
createUGraph edges = accumArray (+) 0 bounds [(symIx e, 1) | e <- edges]
    where bounds = (symIx (minimum vl, minimum vl), symIx (maximum vl, maximum vl))
          vl = foldr (\(x,y) acc -> x:y:acc) [] edges

-- all vertices of a graph
vertices :: UGraph -> [Vertex]
vertices gr = range (vertexBounds gr)

-- all edges of a graph
edges :: UGraph -> [Edge]
edges g = concat [replicate x (pair v) | (v, x) <- assocs g, x /= 0]

-- vertex indices
vertexBounds :: UGraph -> Bounds
vertexBounds gr = (l,h)
    where (SymIx (l,_), SymIx (h,_)) = bounds gr

-- vertices adjacent to another vertex in a graph
adjVertices :: Vertex -> UGraph -> [Vertex]
adjVertices v gr = map (snd) $ filter ((/=) 0 . snd) $ zip verts (adjacency v gr)
    where verts = vertices gr

-- adjacency for a vertex in a graph
adjacency :: Vertex -> UGraph -> [Int]
adjacency v gr = map (gr!) indices
    where indices = matRowIx (vertexBounds gr) v
          matRowIx bnds i = map (\x -> symIx (x,i)) $ range bnds

-- return the degree of a vertex
degree :: Vertex -> UGraph -> Int
degree v = sum . adjacency v

{--------------------------------------------------------------------------------
 -
 - Interna
 -
 -------------------------------------------------------------------------------}

-- symmetric indices
newtype SymIx = SymIx { pair :: (Int, Int) } deriving (Eq, Ord, Show)
instance Ix SymIx where
    range ((SymIx (i1,j1)), (SymIx (i2,j2))) =
        map SymIx [(x,y) | x <- range (i1,i2), y <- range (j1,j2), x >= y]

    inRange (x,y) i = x' <= hx && x' >= lx && y' <= hx && y' >= lx && x' >= y'
        where (lx,ly) = pair x
              (hx,hy) = pair y
              (x',y') = pair i

    index (x,y) i | inRange (x,y) i = (x' - lx) + (sum $ take (y' - ly) [hx-lx, hx - lx - 1..])
                  | otherwise = error "Wrong array index"
                  where (lx, ly) = pair x
                        (hx, hy) = pair y
                        (x', y') = pair i

symIx :: (Int, Int) -> SymIx
symIx (x,y) | x < y = SymIx (y,x)
            | otherwise = SymIx (x,y)

-- returns a row of the upper triangular adjacency matrix of the graph
row :: Vertex -> UGraph -> [Int]
row v gr = [x | (SymIx (i,j), x) <- assocs gr, j == v]

{----------------------------------------------------------------------
 -
 - Isomorphisms and automorphisms of graphs
 -
 - Ref: Practical Graph Isomorphism - B.D.McKak 1981
 -
 ---------------------------------------------------------------------}

type Cell = [Vertex]
-- set of ordered disjoint non-empty cells of a set S with union P = S
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
