module DFS (reachableVertices, isConnected, trivialCycleNodes) where
import Graph
import Util
import Data.Tree
import Control.Monad.State
import Data.Array

{--------------------------------------------------------------------------------
 -
 - Depth first search on graphs and related
 -
 -  * Implementation of DFS equivalent to Data.Graph, but uses custom Graph type
 -  * TODO: biconnected components
 -  * TODO: find cycles in multigraph
 -
 -------------------------------------------------------------------------------}


{--------------------------------------------------------------------------------
 -
 - Public exports
 -
 -------------------------------------------------------------------------------}

-- return a list of all reachable vertices from a given vertex
reachableVertices :: Graph -> Vertex -> [Vertex]
reachableVertices g v = concatMap preorder (depthFirstSearch g [v])

-- a graph is completely connected, if all vertices are reachable from
-- an arbitrary start vertex (here we take the first vertex of the
-- graph)
isConnected :: Graph -> Bool
isConnected gr = length (reachableVertices gr (head vv)) == length vv
    where vv = vertices gr

trivialCycleNodes :: Graph -> [[Vertex]]
trivialCycleNodes = map flatten . depthFirstForest

{--------------------------------------------------------------------------------
 -
 - Internals
 -
 -------------------------------------------------------------------------------}

type Visited = Array Vertex Bool

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
graphToTree :: Vertex -> Graph -> Tree Vertex
graphToTree v gr = Node v (map (`graphToTree` gr) (adjVertices v gr))

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
depthFirstSearch :: Graph -> [Vertex] -> Forest Vertex
depthFirstSearch g v = filterForest bnds (map (`graphToTree` g) v) falseArr
    where filterForest bnds ts = fst . runState (rmDuplVertices ts)
          falseArr = listArray bnds $ repeat False
          bnds = vertexBounds g

-- unordered DFS
depthFirstForest :: Graph -> Forest Vertex
depthFirstForest g = depthFirstSearch g (vertices g)

