module Graph ( Vertex
             , Edge
             , Bounds
             , UGraph
             , isNeighbour
             , createUGraph
             , isMultiGraph
             , hasLoops
             , vertices
             , edges
             , vertexBounds
             , adjacency
             , adjVertices
             , permuteUGraphSymm
             , degree) where

import Data.Array
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
type UGraph = Array SymIx Int

{- Graph Generation -}

-- build a graph from the given edges
createUGraph :: [Edge] -> UGraph
createUGraph edges = accumArray (+) 0 bounds [(symIx e, 1) | e <- edges]
    where bounds = (symIx (minimum vl, minimum vl), symIx (maximum vl, maximum vl))
          vl = foldr (\(x,y) acc -> x:y:acc) [] edges

{- Graph properties -}

-- check if any two vertices are connected directly by more than one edge
isMultiGraph :: UGraph -> Bool
isMultiGraph = any (1 <) . elems

-- check for edges starting and ending in the same vertex
hasLoops :: UGraph -> Bool
hasLoops g = any ((/=) 0) $ [x | (SymIx (i,j), x) <- assocs g, i == j]

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
adjVertices v gr = map (fst) $ filter ((/=) 0 . snd) $ zip verts (adjacency v gr)
    where verts = vertices gr

-- checks if v1 and v2 are directly connected
isNeighbour :: UGraph -> Vertex -> Vertex -> Bool
isNeighbour gr v1 v2 = v1 `elem` (adjVertices v2 gr)

-- adjacency for a vertex in a graph (slowest component in dfs)
-- TODO: avoid construction of list somehow
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

-- perform permutation on UGraph
-- TODO: Replace this *awful* temp solution
permuteUGraphSymm :: [(Int,Int)] -> UGraph -> UGraph
permuteUGraphSymm p g = newgraph
    where arr = array ((l,l), (u,u)) [((x,y), g!(symIx(x,y))) | x <- [l..u], y <- [l..u]]
          newarr = permuteSymmetric p arr
          newgraph = array (SymIx (l,l), SymIx (u,u)) [(symIx (x,y), newarr!(x,y)) | x <- [l..u], y <- [l..u]]
          (SymIx (l,_), SymIx (u,_)) = bounds g

permuteRows :: [(Int,Int)] -> Array (Int, Int) a -> Array (Int, Int) a
permuteRows x m = m // [a | k <- [l..u], (i,n) <- x, a <- [((i,k), m!(n,k))]]
    where ((_,l), (_,u)) = bounds m

permuteCols :: [(Int,Int)] -> Array (Int, Int) a -> Array (Int, Int) a
permuteCols x m = m // [a | k <- [l..u], (i,n) <- x, a <- [((k,i), m!(k,n))]]
    where ((l,_), (u,_)) = bounds m

permuteSymmetric :: [(Int,Int)] -> Array (Int, Int) a -> Array (Int, Int) a
permuteSymmetric x = permuteRows x . permuteCols x




