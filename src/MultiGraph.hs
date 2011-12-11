module MultiGraph
             ( Vertex
             , Edge
             , Bounds
             , MultiGraph (MultiGraph)
             , Row
             , adjCompare
             , getAdj
             , isNeighbour
             , degreeNeighbour
             , createGraph
             , hasLoops
             , nLoops
             , vertices
             , nVertices
             , edges
             , adjacency
             , adjVertices
             , adjVerticesWReps
             , vertexBounds
             , permuteGraphSymm
             , degree
             , degreeSequence) where

import Control.Monad.State
import Data.Tree
import Data.List
import Data.Array
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as V
import Data.Function (on)
import Util

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
data MultiGraph = MultiGraph { vertexBounds :: (Int, Int), getAdj :: AdjMatrix } deriving Show

-- comparison and equality tests by adjacency matrix
instance Eq MultiGraph where
    x == y = getAdj x == getAdj y

instance Ord MultiGraph where
    x `compare` y =  getAdj x `compare` getAdj y


createGraph :: [Edge] -> MultiGraph
createGraph edges = MultiGraph bounds $ V.unfoldr buildAdj (range bounds)
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

getElem :: MultiGraph -> Vertex -> Vertex -> Int
getElem g v1 = (UV.!) (adjacency v1 g)

vertices :: MultiGraph -> [Vertex]
vertices = range . vertexBounds

nVertices :: MultiGraph -> Int
nVertices g = snd v - fst v + 1
    where v = vertexBounds g

-- check for edges starting and ending in the same vertex
hasLoops :: MultiGraph -> Bool
hasLoops g = any (0 /=) [getElem g i i | i <- vertices g]


-- give the number of loops
nLoops :: MultiGraph -> Int
nLoops g = sum $ filter (0 /=) [getElem g i i | i <- vertices g]

-- all edges of a graph
edges :: MultiGraph -> [Edge]
edges g = concat [replicate (getElem g i j) (i, j) | i <- vertices g, j <- range (i, snd $ vertexBounds g) , getElem g i j /= 0]

-- adjacent vertices to a given vertex
adjVertices :: Vertex -> MultiGraph -> Row
adjVertices v g = UV.findIndices ((/=) 0) (adjacency v g)

-- adjacent vertices to a given vertex including repetitions for
-- parallel edges
adjVerticesWReps :: Vertex -> MultiGraph -> [Vertex]
adjVerticesWReps v g = fl
    where ll = UV.toList $ adjacency v g
          fl = concatMap (\(v,r) -> replicate r v) $ zip [0..] ll

-- checks if v1 and v2 are directly connected
isNeighbour :: MultiGraph -> Vertex -> Vertex -> Bool
isNeighbour gr v1 v2 = v1 `UV.elem` adjVertices v2 gr

degreeNeighbour :: MultiGraph -> Vertex -> Vertex -> Int
degreeNeighbour g v1 v2 | v1 == v2 = (*)(-1) $ UV.unsafeIndex adj v2
                        | otherwise = UV.unsafeIndex adj v2
    where adj = adjacency v1 g

-- adjacency for a vertex in a graph (slowest component in dfs)
-- TODO: avoid construction of list somehow
adjacency :: Vertex -> MultiGraph -> Row
adjacency v = flip (V.unsafeIndex) v . getAdj

-- return the degree of a vertex
degree :: Vertex -> MultiGraph -> Int
degree v g = (UV.unsafeIndex) adj v + UV.sum adj
    where adj = adjacency v g

degreeSequence :: MultiGraph -> [(Vertex, Int)]
degreeSequence g = sortBy (compare `on` fst) $
                   occurences $
                   map (flip degree g) (vertices g)

adjCompare :: MultiGraph -> MultiGraph -> Ordering
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
permuteGraphSymm :: [(Int,Int)] -> MultiGraph -> MultiGraph
permuteGraphSymm p g = MultiGraph (vertexBounds g) permuteAll
    where adj = getAdj g
          perm = V.accum (+) (V.replicate (V.length adj) 0) p
          permUV = UV.accum (+) (UV.replicate (V.length adj) 0) p
          permuteRows = V.backpermute adj perm
          permuteAll = V.map (\x -> UV.backpermute x permUV) permuteRows
