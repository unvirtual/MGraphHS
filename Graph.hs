module Graph ( Vertex
             , Edge
             , Bounds
             , Graph
             , adjCompare
             , getAdj
             , isNeighbour
             , degreeNeighbour
             , createGraph
             , hasLoops
             , vertices
             , nVertices
             , edges
             , adjacency
             , adjVertices
             , vertexBounds
             , permuteGraphSymm
             , degree) where

import Control.Monad.State
import Data.Tree
import Data.List
import Data.Array
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as V

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
type Row = UV.Vector Int
type AdjMatrix = V.Vector Row
data Graph = Graph { vertexBounds :: (Int, Int), getAdj :: AdjMatrix } deriving Show

createGraph :: [Edge] -> Graph
createGraph edges = Graph bounds $ V.unfoldr buildAdj (range bounds)
    where bounds = (minimum vl, maximum vl)
          vl = foldr (\(x,y) acc -> x:y:acc) [] edges
          buildAdj :: [Vertex] -> Maybe (Row, [Vertex])
          buildAdj [] = Nothing
          buildAdj (v:vs) = Just (rr,vs)
              where rr = UV.unsafeAccum (+) zeroVec [rowelement x v | x <- edges]
                    zeroVec = UV.replicate (snd bounds - fst bounds + 1) 0
          rowelement edge v | fst edge == v = (snd edge, 1)
                            | snd edge == v = (fst edge, 1)
                            | otherwise = (0,0)

getElem :: Graph -> Vertex -> Vertex -> Int
getElem g v1 = (UV.!) (adjacency v1 g)

vertices :: Graph -> [Vertex]
vertices = range . vertexBounds

nVertices :: Graph -> Int
nVertices g = snd v - fst v + 1
    where v = vertexBounds g

-- check for edges starting and ending in the same vertex
hasLoops :: Graph -> Bool
hasLoops g = any (0 /=) [getElem g i i | i <- vertices g]

-- all edges of a graph
edges :: Graph -> [Edge]
edges g = concat [replicate (getElem g i j) (i, j) | i <- vertices g, j <- range (i, snd $ vertexBounds g) , getElem g i j /= 0]


adjVertices :: Vertex -> Graph -> [Vertex]
adjVertices v g = [x | x <- vertices g, getElem g v x /= 0]

-- checks if v1 and v2 are directly connected
isNeighbour :: Graph -> Vertex -> Vertex -> Bool
isNeighbour gr v1 v2 = v1 `elem` adjVertices v2 gr

degreeNeighbour :: Graph -> Vertex -> Vertex -> Int
degreeNeighbour g v1 v2 | v1 == v2 = (*)(-1) $ (UV.!) adj v2
                         | otherwise = (UV.!) adj v2
    where adj = adjacency v1 g

-- adjacency for a vertex in a graph (slowest component in dfs)
-- TODO: avoid construction of list somehow
adjacency :: Vertex -> Graph -> Row
adjacency v = flip (V.!) v . getAdj

-- return the degree of a vertex
degree :: Vertex -> Graph -> Int
degree v g = (UV.!) adj v + UV.sum adj
    where adj = adjacency v g

adjCompare :: Graph -> Graph -> Ordering
adjCompare g1 g2 | arr1 == arr2 = EQ
                 | otherwise    = diff $ diffElems arr1 arr2
    where arr1 = getAdj g1
          arr2 = getAdj g2
          diffElems a1 a2 = [(getElem g1 i j, getElem g2 i j) |
                             i <- vertices g1,
                             j <- vertices g2,
                             getElem g1 i j /= getElem g2 i j]
          diff :: (Ord a) => [(a,a)] -> Ordering
          diff x = case x of
              [] -> EQ
              ((a,b):xs) -> a `compare` b

-- perform permutation on UGraph
-- TODO: Replace this *awful* temp solution
permuteGraphSymm :: [(Int,Int)] -> Graph -> Graph
permuteGraphSymm p g = Graph (vertexBounds g) (permuteSymmetric p (getAdj g))

permuteRows :: [(Int,Int)] -> AdjMatrix -> AdjMatrix
permuteRows x m = (V.//) m (concat [[(i, (V.!) m j), (j, (V.!) m i)] | (i,j) <- x])

permuteCols :: [(Int, Int)] -> AdjMatrix -> AdjMatrix
permuteCols x m = V.map (f x) m
    where f x r = (UV.//) r (concat [[(i, (UV.!) r j), (j, (UV.!) r i)] | (i,j) <- x])

permuteSymmetric :: [(Int, Int)] -> AdjMatrix -> AdjMatrix
permuteSymmetric x = permuteRows x . permuteCols x
--
--permuteCols :: [(Int,Int)] -> Array (Int, Int) a -> Array (Int, Int) a
--permuteCols x m = m // [a | k <- [l..u], (i,n) <- x, a <- [((k,i), m!(k,n))]]
--    where ((l,_), (u,_)) = bounds m
--
--permuteSymmetric :: [(Int,Int)] -> Array (Int, Int) a -> Array (Int, Int) a
--permuteSymmetric x = permuteRows x . permuteCols x
