module MultiGraph.DFS
    ( reachableVertices
    , isConnected
    , nCycles
    , trivialCycleNodes
    , depthFirstForest) where

import MultiGraph
import Util
import Data.Tree
import Control.Monad.State
import Data.Array
import Control.Monad.ST
import qualified Data.Array.ST as STA
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as V

import Data.STRef

import Debug.Trace

{--------------------------------------------------------------------------------
 -
 - Depth first search on graphs and related
 -
 -  * Implementation of DFS equivalent to Data.Graph, but uses custom Graph type
 -  * TODO: biconnected components
 -
 -------------------------------------------------------------------------------}

{--------------------------------------------------------------------------------
 -
 - Public exports
 -
 -------------------------------------------------------------------------------}

-- return a list of all reachable vertices from a given vertex
reachableVertices :: MultiGraph -> Vertex -> [Vertex]
reachableVertices g v = concatMap preorder (depthFirstSearch g [v])

-- a graph is completely connected, if all vertices are reachable from
-- an arbitrary start vertex (here we take the first vertex of the
-- graph)
isConnected :: MultiGraph -> Bool
isConnected gr = length (reachableVertices gr (head vv)) == length vv
    where vv = vertices gr

-- determine the number of loops in thq QFT sense
nCycles :: MultiGraph -> Int
nCycles g = nLoops g + nChordCycles g

trivialCycleNodes :: MultiGraph -> [[Vertex]]
trivialCycleNodes = map flatten . depthFirstForest

{--------------------------------------------------------------------------------
 -
 - Internals
 -
 -------------------------------------------------------------------------------}

type Visited = Array Vertex Bool
type VisitedEdges = (Array Vertex Bool, [(Int, Int)], Int)

-- preorder of a B-tree
preorder :: Tree a -> [a]
preorder (Node x xs) = x:concatMap preorder xs

-- postorder of a B-tree
postorder :: Tree a -> [a]
postorder (Node x xs) = concatMap postorder xs ++ [x]

-- create a tree from an Graph given a root vertex. This tree then
-- contains all reachable vertices from the given vertex and is
-- infinte in size. It has to be filtered, such that every vertex
-- appears only once as a node, using depth-first search.
graphToTree :: Vertex -> MultiGraph -> Tree Vertex
graphToTree v gr = Node v (map (`graphToTree` gr) (UV.toList $ adjVertices v gr))

-- remove duplicate vertices in a forest of vertex trees
rmDuplVertices :: Forest Vertex -> State Visited (Forest Vertex)
rmDuplVertices [] = return []
rmDuplVertices (Node v rest:frst) = do
    visited <- get
    if visited!v then rmDuplVertices frst else
        (do modify (\x -> x // [(v,True)])
            redRest <- rmDuplVertices rest
            redFrst <- rmDuplVertices frst
            return (Node v redRest:redFrst))

-- perform depth-first search on a graph
depthFirstSearch :: MultiGraph -> [Vertex] -> Forest Vertex
depthFirstSearch g v = filterForest bnds (map (`graphToTree` g) v) falseArr
    where filterForest bnds ts = fst . runState (rmDuplVertices ts)
          falseArr = listArray bnds $ repeat False
          bnds = vertexBounds g

-- unordered DFS
depthFirstForest :: MultiGraph -> Forest Vertex
depthFirstForest g = depthFirstSearch g (vertices g)

-- determine the number of chord cycles
nChordCycles :: MultiGraph -> Int
nChordCycles g = runST $ do
    arr <- STA.newArray (vertexBounds g) False :: ST s (STA.STUArray s Vertex Bool)
    edgs <- newSTRef (edges g)
    cycles <- newSTRef 0

    let updateEdges v v0 = do
         e <- readSTRef edgs
         let se = symTupleElem (v,v0) e
         case se of
            Just (a,b) -> do modifySTRef edgs (removeFirst (a,b))
                             return True
            Nothing    -> do return False

    let markVertex v0 v = do
         seenVertex <- STA.readArray arr v
         updatedEdge <- updateEdges v0 v
         if seenVertex
             then when updatedEdge $ do modifySTRef cycles (+1)
             else do STA.writeArray arr v True

    let findNCycles v = do let adj = filter (\x -> x /= v) $ adjVerticesWReps v g
                           mapM (markVertex v) adj

    let order = concatMap preorder $ depthFirstForest g
    mapM (findNCycles) order
    c <- readSTRef cycles
    return c
