module Graph ( Vertex
             , Edge
             , Bounds
             , UGraph
             , adjCompare
             , getArray
             , isNeighbour
             , degreeNeighbour
             , createUGraph
             , isMultiGraph
             , hasLoops
             , vertices
             , nvertices
             , edges
             , vertexBounds
             , adjacency
             , adjVertices
             , permuteUGraphSymm
             , degree) where

import Control.Monad.State
import Data.Tree
import Data.List
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
newtype UGraph = UGraph { getArray :: Array SymIx Int } deriving (Show)

{- Graph Generation -}

-- build a graph from the given edges
createUGraph :: [Edge] -> UGraph
createUGraph edges = UGraph $ accumArray (+) 0 bounds [(symIx e, 1) | e <- edges]
    where bounds = (symIx (minimum vl, minimum vl), symIx (maximum vl, maximum vl))
          vl = foldr (\(x,y) acc -> x:y:acc) [] edges

{- Graph properties -}

-- check if any two vertices are connected directly by more than one edge
isMultiGraph :: UGraph -> Bool
isMultiGraph = any (1 <) . elems . getArray

-- check for edges starting and ending in the same vertex
hasLoops :: UGraph -> Bool
hasLoops g = any (0 /=) [x | (SymIx (i,j), x) <- assocs $ getArray g, i == j]

-- all vertices of a graph
vertices :: UGraph -> [Vertex]
vertices gr = range (vertexBounds gr)

-- all edges of a graph
edges :: UGraph -> [Edge]
edges g = concat [replicate x (pair v) | (v, x) <- assocs $ getArray g, x /= 0]

-- vertex indices
vertexBounds :: UGraph -> Bounds
vertexBounds gr = (l,h)
    where (SymIx (l,_), SymIx (h,_)) = bounds $ getArray gr

-- number of vertices
nvertices :: UGraph -> Int
nvertices = length . vertices

-- vertices adjacent to another vertex in a graph
adjVertices :: Vertex -> UGraph -> [Vertex]
adjVertices v g = [x | x <- vertices g, getArray g!symIx (x,v) /= 0]

-- checks if v1 and v2 are directly connected
isNeighbour :: UGraph -> Vertex -> Vertex -> Bool
isNeighbour gr v1 v2 = v1 `elem` adjVertices v2 gr

degreeNeighbour :: UGraph -> Vertex -> Vertex -> Int
degreeNeighbour g v1 v2 | v1 == v2 = -adj
                        | otherwise = adj
    where adj = getArray g!symIx (v1,v2)

-- adjacency for a vertex in a graph (slowest component in dfs)
-- TODO: avoid construction of list somehow
adjacency :: Vertex -> UGraph -> [Int]
adjacency v g = [getArray g!symIx (x,v) | x <- vertices g]

-- return the degree of a vertex
degree :: Vertex -> UGraph -> Int
degree v g = (adj!!v) +  sum adj
    where adj = adjacency v g

-- compare the adjacency matrices of the given graph
adjCompare :: UGraph -> UGraph -> Ordering
adjCompare g1 g2 | arr1 == arr2 = EQ
                 | otherwise    = diff $ diffElems arr1 arr2
    where arr1 = getArray g1
          arr2 = getArray g2
          diffElems a1 a2 = [(a1!symIx (i,j), a2!symIx (i,j)) |
                             i <- range $ vertexBounds g1,
                             j <- range $ vertexBounds g2,
                             a1!symIx (i,j) /= a2!symIx (i,j)]
          diff :: (Ord a) => [(a,a)] -> Ordering
          diff x = case x of
              [] -> EQ
              ((a,b):xs) -> a `compare` b

{--------------------------------------------------------------------------------
 -
 - Interna
 -
 -------------------------------------------------------------------------------}

-- symmetric indices
newtype SymIx = SymIx { pair :: (Int, Int) } deriving (Eq, Ord, Show)
instance Ix SymIx where
    range (SymIx (i1,j1), SymIx (i2,j2)) =
        map SymIx [(x,y) | x <- range (i1,i2), y <- range (j1,j2), x >= y]

    inRange (x,y) i = x' <= hx && x' >= lx && y' <= hx && y' >= lx && x' >= y'
          where (lx, ly) = pair x
                (hx, hy) = pair y
                (x', y') = pair i
    index (x,y) i | inRange (x,y) i = (x' - lx) + sum (take (y' - ly) [hx-lx, hx - lx - 1..])
                  | otherwise = error "Wrong array index"
          where (lx, ly) = pair x
                (hx, hy) = pair y
                (x', y') = pair i

symIx :: (Int, Int) -> SymIx
symIx (x,y) | x < y = SymIx (y,x)
            | otherwise = SymIx (x,y)

-- returns a row of the upper triangular adjacency matrix of the graph
row :: Vertex -> UGraph -> [Int]
row v gr = [x | (SymIx (i,j), x) <- assocs $ getArray gr, j == v]

-- perform permutation on UGraph
-- TODO: Replace this *awful* temp solution
permuteUGraphSymm :: [(Int,Int)] -> UGraph -> UGraph
permuteUGraphSymm p g = UGraph newgraph
    where arr = array ((l,l), (u,u)) [((x,y), getArray g!symIx(x,y)) | x <- [l..u], y <- [l..u]]
          newarr = permuteSymmetric p arr
          newgraph = array (SymIx (l,l), SymIx (u,u)) [(symIx (x,y), newarr!(x,y)) | x <- [l..u], y <- [l..u]]
          (SymIx (l,_), SymIx (u,_)) = bounds $ getArray g

permuteRows :: [(Int,Int)] -> Array (Int, Int) a -> Array (Int, Int) a
permuteRows x m = m // [a | k <- [l..u], (i,n) <- x, a <- [((i,k), m!(n,k))]]
    where ((_,l), (_,u)) = bounds m

permuteCols :: [(Int,Int)] -> Array (Int, Int) a -> Array (Int, Int) a
permuteCols x m = m // [a | k <- [l..u], (i,n) <- x, a <- [((k,i), m!(k,n))]]
    where ((l,_), (u,_)) = bounds m

permuteSymmetric :: [(Int,Int)] -> Array (Int, Int) a -> Array (Int, Int) a
permuteSymmetric x = permuteRows x . permuteCols x
