module Morphisms (refine, canonicGraph, isIsomorphic) where

import Graph
import Util
import DFS
import Data.Array
import Data.Tree
import Data.Int
import Data.Bits
import qualified Data.Vector.Unboxed as UV
import Control.Monad.ST
import Data.STRef

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

-- check if twop graphs are isomorphic
isIsomorphic :: Graph -> Graph -> Bool
isIsomorphic g1 g2 = (v1 == v2) && arr g1 v1 == arr g2 v2
                     where (v1,v2) = (vertices g1, vertices g2)
                           arr g v = getAdj $ canonicGraph g [v]

-- return the automorphisms of a given graph together with the canonically
-- labeled graph
automorphisms :: Partition -> Graph -> (Graph, [Relabeling])

-- permute the adjacency matrix and return the relabeled graph
canonicGraph :: Graph -> Partition -> Graph
canonicGraph g p = fst $ automorphisms p g

-- refine a partition with respect to another partition
refine :: Graph -> Partition -> [Cell] -> Partition
refine gr pi ww = case ww of
    [] -> pi
    (w:ws) -> refine gr pinew wsnew
              where (pinew, wsnew) = refinePi gr pi w ws

{---------------------------------------------------------------------
 -
 - Internal
 -
 ---------------------------------------------------------------------}

-- now we can define the Graph to be instances of Eq and Ord
instance Eq Graph where
    x == y = isIsomorphic x y

instance Ord Graph where
    x `compare` y =  graphCompare x y

-- comparison of graphs after determining the canonical ordering
graphCompare :: Graph -> Graph -> Ordering
graphCompare g1 g2 | nVertices g1 /= nVertices g2 = nVertices g1 `compare` nVertices g2
                   | otherwise = adjCompare cg1 cg2
    where cg1 = canonicGraph g1 (unitPartition (vertexBounds g1))
          cg2 = canonicGraph g2 (unitPartition (vertexBounds g2))

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
isDiscrete = all isTrivial

isUnit :: Partition -> Bool
isUnit = (==) 1 . length

fix :: Partition -> [Vertex]
fix = concat . filter isTrivial

supp :: Partition -> [Vertex]
supp = concat . filter (not . isTrivial)

-- {min V_i | V_i \in partition}
-- Assumes that the vertices in each cell are sorted in increasing
-- order
minimumCellRep :: Partition -> [Vertex]
minimumCellRep = map head

cellMapping :: Partition -> Partition -> [Edge]
cellMapping p q = zip (minimumCellRep p) (minimumCellRep q)


{----------------------------------------------------------------------
 -
 - Partition refinement
 -
 ----------------------------------------------------------------------}

-- refine a given current pi elem Pi_G recursively given a w and ws,
-- return the new pi and the modified ws
refinePi :: Graph -> Partition -> Cell -> [Cell] -> (Partition, [Cell])
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
refinePiE :: Graph -> Cell -> Cell -> [Cell] -> (Partition, [Cell])
refinePiE gr pie wi ws
    | isUnit xx = ([pie], ws)
    | otherwise = (xx, wsmod)
    where xx = orderedPartition gr wi pie
          (maxX, restX) = splitLongest xx
          wsmod = replaceElem pie maxX ws ++ restX

-- Given a cell W find X = (X_1,..,X_s) which is an element of all partition on a
-- given cell V such that d(x,W) < d(y,W) for x in X_i and y in X_j if i<j
orderedPartition :: Graph -> Cell -> Cell -> Partition
orderedPartition gr w = groupSort (cellDegree gr w)

-- number of vertices in a cell adjacent to a given vertex
cellDegree :: Graph -> Cell -> Vertex -> Int
cellDegree gr c v = sum $ map (degreeNeighbour gr v) c

-- the scalar product pi.v of a partition with a vertex is defined as
-- extracting the vertex from its cell and inserting it as a trivial
-- cell before the matching cell
parallelProject :: Partition -> Vertex -> Partition
parallelProject [] _ = []
parallelProject (p:ps) v | (not . isTrivial) p && vInCell v p = [v]:extractV v p:ps
                         | otherwise = p : parallelProject ps v
    where vInCell v c = v `elem` c
          extractV _ [] = []
          extractV v (c:cs) | c == v = cs
                            | otherwise = c : extractV v cs

-- the orthogonal projection pi \ortho v of a partition and a vertex
-- is defined as R(G, pi.v, [[v]])
orthoProject :: Graph -> Partition -> Vertex -> Partition
orthoProject gr pi v = refine gr (parallelProject pi v) [[v]]

-- next level of descendants of given parition
descendantPartitions :: Graph -> Partition -> [(Vertex, Partition)]
descendantPartitions g p = dp g p (verts p)
    where verts vv | isDiscrete vv = []
                   | otherwise= head $ dropWhile (isTrivial) vv
          dp g p vv = zip vv $ map (orthoProject g p) vv

-- trivial indicator function (hash for given partition)
indicator :: Graph -> Partition -> Int32
indicator g = orderedHash . map unorderedHash . inner
    where inner = foldr
                  (\p -> (:) (map (fromIntegral . cellDegree g p) (vertices g))) []
          -- order independent hash of a vector
          unorderedHash = foldl xor 0
          -- rotating hash
          orderedHash = foldl (\acc x -> (shift acc 7 `xor` shift acc (-28) `xor` x)) 0

{----------------------------------------------------------------------
 -
 - Relabeling
 -
 ----------------------------------------------------------------------}

type Relabeling = UV.Vector Int

-- create a relabeling
toRelabeling :: Bounds -> [(Int, Int)] -> Relabeling
toRelabeling (l,u) list = UV.accum (+) (UV.replicate (u-l+1) 0) list

-- vertices invariant under given Relabeling
invariantVertices :: Relabeling -> [Vertex]
invariantVertices = UV.toList . UV.ifilter (\i v -> i == v)

-- relabel a partition with respect to another one
partitionRelabeling :: Bounds -> Partition -> Partition -> Relabeling
partitionRelabeling bnds p q = toRelabeling bnds $ cellMapping p q

-- Find cycles in Relabeling and return the resulting partition
groupCycles :: Relabeling -> Partition
groupCycles = trivialCycleNodes . createGraph . UV.toList . UV.indexed

zeroVec :: Bounds -> UV.Vector Int
zeroVec (l,u) = UV.replicate (u-l+1) 0

-- combine two relabellings
combineRelabelings :: Relabeling -> Relabeling -> Relabeling
combineRelabelings p q = UV.accum (+) (zeroVec (0, UV.length p)) $
                         concatMap getCycle (trivialCycleNodes graph)
    where next l x = case x of
                (v1:v2:vs) -> (v1,v2) : next l (v2:vs)
                (v:[])     -> [(v,l)]
                _          -> []
          getCycle x = case x of
                (xs:xss) -> next xs x
                _        -> []
          graph = createGraph (zipWith (\x y -> (x,y)) (UV.toList p) (UV.toList q))

-- minimum cell representation from a given Relabeling
mcrFromRelabeling :: Relabeling -> [Vertex]
mcrFromRelabeling = minimumCellRep . groupCycles

{----------------------------------------------------------------------
 -
 - Find automorphisms
 -
 ----------------------------------------------------------------------}

type Leaf = (Partition, [Int32], [Vertex])

-- get the leaf that is to the far left for the given graph and
-- partition
leftLeaf :: Graph -> Partition -> Leaf
leftLeaf g p = case desc of
                    ((v,pp):_) -> let (newp, inds, visited) = leftLeaf g pp
                                  in (newp, indicator g p : inds, v : visited)
                    []         -> (p, [indicator g p], [])
               where desc = descendantPartitions g p

-- find the vertex where two paths diverge and check if this vertex is
-- contained in the third list
pathDiffInList :: [Vertex] -> [Vertex] -> [Vertex] -> Bool
pathDiffInList l1 l2 c | null dropped = True
                       | otherwise    = (head dropped) `elem` c
    where common   = commonElemsAtStart l1 l2
          dropped  = drop common l2

-- relabel a graph with respect to the given Partition
relabelGraph :: Graph -> Partition -> Graph
relabelGraph g p = createGraph (newedges relabeling)
    where relabeling = UV.accum (+) (zeroVec bnds) $ zip (map head p) (range bnds)
          newedges r = map (\(v1,v2) -> ((UV.!) r v1, (UV.!) r v2)) $ edges g
          bnds = vertexBounds g


checkNode v vis ams = all (elem v) [mcrFromRelabeling i| i <- drop 1 ams,
                                    isSubsetOf vis $ invariantVertices i]

newLabels g (t,_) = let tm = combineRelabelings g
                        mcro = mcrFromRelabeling . tm
                    in (tm t, mcro t)

automorphisms p g = runST $ do
    -- mutable state
    automorphisms <- newSTRef []
    labels <- newSTRef (UV.fromList (vertices g), vertices g)
    let tree = refine g p p
    let (leafP, leafI, leafVisited) = leftLeaf g tree
    let leafG =  relabelGraph g leafP
    currentP <- newSTRef leafP
    currentI <- newSTRef leafI
    currentG <- newSTRef leafG

    let updateAutoM vv p = do
        let gamma = partitionRelabeling (vertexBounds g) vv p
        modifySTRef automorphisms (gamma:)
        modifySTRef labels (newLabels gamma)

    let nextNode partition visited indicators = do
        let traverseDescendants nodes = do
            let traverseStep (n:ns) = do
                ams <- readSTRef automorphisms
                if (checkNode (fst n) visited ams)
                   then nextNode (snd n)
                                 (fst n:visited)
                                 (indicator g (snd n):indicators)
                   else do return ()
                 ---- changes here!!
                (_, xx) <- readSTRef labels
                if pathDiffInList leafVisited visited xx
                    then do traverseDescendants ns
                    else do return ()

            case nodes of
                []     -> do return ()
                (n:ns) -> do traverseStep (n:ns)

        let update cG = do
            currentPV <- readSTRef currentP
            currentIV <- readSTRef currentI
            currentGV <- readSTRef currentG
            let cmp (x,gx) (y,gy) | x == y && (getAdj gx) == (getAdj gy)   = do 
                                         updateAutoM currentPV partition
                                  | x < y  && (getAdj gx) < (getAdj gy)   = do
                                         writeSTRef currentP partition
                                         writeSTRef currentI indicators
                                         writeSTRef currentG cG
                                  | otherwise = do return ()
            cmp (indicators, cG) (currentIV, currentGV)

        let termNode = do
            let cG = relabelGraph g partition
            if indicators == leafI && getAdj cG == getAdj leafG
                then updateAutoM partition leafP
                else update cG

        currentIV <- readSTRef currentI
        if (startsWith leafI indicators || indicators <= currentIV)
            then do
               let desc = descendantPartitions g partition
               case desc of
                    [] -> do termNode
                    (x:xs) -> traverseDescendants desc
            else do return ()

    -- run the tree traversal
    nextNode tree [] [indicator g tree]

    ams <- readSTRef automorphisms
    canonical <- readSTRef currentG
    return (canonical, ams)
