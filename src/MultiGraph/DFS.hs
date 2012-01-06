module MultiGraph.DFS
    ( reachableNodes
    , isConnected
    , nCycles
    , fstInChordCycles
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

-- return a list of all reachable nodes from a given node
reachableNodes :: MultiGraph -> Node -> [Node]
reachableNodes g v = concatMap preorder (depthFirstSearch g [v])

-- a graph is completely connected, if all nodes are reachable from
-- an arbitrary start node (here we take the first node of the
-- graph)
isConnected :: MultiGraph -> Bool
isConnected gr = length (reachableNodes gr (head vv)) == length vv
    where vv = nodes gr

-- determine the number of loops in thq QFT sense
nCycles :: MultiGraph -> Int
nCycles = length . fstInChordCycles

trivialCycleNodes :: MultiGraph -> [[Node]]
trivialCycleNodes = map flatten . depthFirstForest

{--------------------------------------------------------------------------------
 -
 - Internals
 -
 -------------------------------------------------------------------------------}

type Visited = Array Node Bool
type VisitedEdges = (Array Node Bool, [(Int, Int)], Int)

-- preorder of a B-tree
preorder :: Tree a -> [a]
preorder (Node x xs) = x:concatMap preorder xs

-- postorder of a B-tree
postorder :: Tree a -> [a]
postorder (Node x xs) = concatMap postorder xs ++ [x]

-- create a tree from an Graph given a root node. This tree then
-- contains all reachable nodes from the given node and is
-- infinte in size. It has to be filtered, such that every node
-- appears only once as a node, using depth-first search.
graphToTree :: Node -> MultiGraph -> Tree Node
graphToTree v gr = Node v (map (`graphToTree` gr) (UV.toList $ adjNodes v gr))

-- remove duplicate nodes in a forest of node trees
rmDuplNodes :: Forest Node -> State Visited (Forest Node)
rmDuplNodes [] = return []
rmDuplNodes (Node v rest:frst) = do
    visited <- get
    if visited!v then rmDuplNodes frst else
        (do modify (\x -> x // [(v,True)])
            redRest <- rmDuplNodes rest
            redFrst <- rmDuplNodes frst
            return (Node v redRest:redFrst))

-- perform depth-first search on a graph
depthFirstSearch :: MultiGraph -> [Node] -> Forest Node
depthFirstSearch g v = filterForest bnds (map (`graphToTree` g) v) falseArr
    where filterForest bnds ts = fst . runState (rmDuplNodes ts)
          falseArr = listArray bnds $ repeat False
          bnds = nodeBounds g

-- unordered DFS
depthFirstForest :: MultiGraph -> Forest Node
depthFirstForest g = depthFirstSearch g (nodes g)

-- return the first edge in each cycle. Length of result corresponds to the
-- total number of cycles
fstInChordCycles :: MultiGraph -> [Edge]
fstInChordCycles g = fstInNontrivialCycles g ++ loopEdges g

fstInNontrivialCycles :: MultiGraph -> [Edge]
fstInNontrivialCycles g = runST $ do
    arr <- STA.newArray (nodeBounds g) False :: ST s (STA.STUArray s Node Bool)
    edgs <- newSTRef (edges g)
    cycles <- newSTRef 0
    cycleFirstEdges <- newSTRef []

    let updateEdges v v0 = do
         e <- readSTRef edgs
         let se = symTupleElem (v,v0) e
         case se of
            Just (a,b) -> do modifySTRef edgs (removeFirst (a,b))
                             return True
            Nothing    -> do return False

    let markNode v0 v = do
         seenNode <- STA.readArray arr v
         updatedEdge <- updateEdges v0 v
         if seenNode
             then when updatedEdge $ do
                      modifySTRef cycles (+1)
                      modifySTRef cycleFirstEdges ((v0,v):)
             else do STA.writeArray arr v True

    let findNCycles v = do let adj = filter (\x -> x /= v) $ adjNodesWReps v g
                           mapM (markNode v) adj

    let order = concatMap preorder $ depthFirstForest g
    mapM (findNCycles) order
    c <- readSTRef cycleFirstEdges
    return c
