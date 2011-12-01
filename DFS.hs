module DFS (reachableVertices, isConnected) where
import Graph
import Util
import Data.Tree
import Control.Monad.State
import Data.Array
import qualified Data.Array.Unboxed as UA
import qualified Data.Vector.Unboxed as UV
import Data.Array.ST

import Control.Monad.ST

{--------------------------------------------------------------------------------
 -
 - Depth first search on graphs and related
 -
 - TODO: use mutable array for performance
 -
 -------------------------------------------------------------------------------}

{--------------------------------------------------------------------------------
 -
 - Public exports
 -
 -------------------------------------------------------------------------------}

-- return a list of all reachable vertices from a given vertex
reachableVertices :: Graph -> Vertex -> [Vertex]
reachableVertices g v = concatMap preorder (depthFirst g [v])

-- a graph is completely connected, if all vertices are reachable from
-- an arbitrary start vertex (here we take the first vertex of the
-- graph)
isConnected :: Graph -> Bool
isConnected gr = length (reachableVertices gr (head vv)) == length vv
    where vv = vertices gr

{--------------------------------------------------------------------------------
 -
 - Internals
 -
 -------------------------------------------------------------------------------}

type Visited s = STUArray s Vertex Bool


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
uGraphToTree :: Vertex -> Graph -> Tree Vertex
uGraphToTree v gr = Node v (map (`uGraphToTree` gr) (UV.toList $ adjVertices v gr))

-- remove duplicate vertices in a forest of vertex trees
rmDuplVertices :: Visited s -> Forest Vertex -> ST s (Forest Vertex)
rmDuplVertices _ [] = return []
rmDuplVertices vis (Node v rest:frst) = do
    isSeen <- readArray vis v
    if isSeen
        then do rmDuplVertices vis frst
        else do writeArray vis v True
                r1 <- rmDuplVertices vis rest
                r2 <- rmDuplVertices vis frst
                return ((Node v r1):r2)

-- perform depth-first search on a graph
depthFirst :: Graph -> [Vertex] -> Forest Vertex
depthFirst g v = filterForest (map (`uGraphToTree` g) v)
    where filterForest ts = runST $ do
                arr <- newArray bnds False
                rmDuplVertices arr ts
          bnds = vertexBounds g
