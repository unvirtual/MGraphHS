module Graph ( Vertex
             , Edge
             , Bounds
             , UGraph
             , createUGraph
             , degreeGraphs
             , isMultiGraph
             , hasLoops
             , vertices
             , edges
             , vertexBounds
             , adjacency
             , reachableVertices
             , isConnected
             , adjVertices
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

-- generate all graphs for a given degree sequence [(Degree, #vertices)]
-- TODO: Should return [UGraph]
degreeGraphs :: [(Int,Int)] -> [UGraph]
degreeGraphs degreeSeq = map adjMatToUGraph $ map snd reduction
    where p = initPartition degreeSeq
          reduction = genDegGraphs (p, initIAdj (ecPartitionVertices p))
          initIAdj :: Int -> AdjMat
          initIAdj n = replicate n []
          -- temporary helper function
          adjMatToUGraph :: AdjMat -> UGraph
          adjMatToUGraph m = createUGraph (amEdges m)
              where amEdges m = [(fst x, y) | x <- zz, y <- snd x]
                                where zz = zip [0..] m

-- generate all graphs on `n` vertices with all possible combinations
-- of degrees `degrees`
allGraphs :: Int -> [Int] -> [UGraph]
allGraphs nvert degrees = undefined

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

-- return a list of all reachable vertices from a given vertex
reachableVertices :: UGraph -> Vertex -> [Vertex]
reachableVertices g v = concatMap preorder (depthFirst g [v])

-- a graph is completely connected, if all vertices are reachable from
-- an arbitrary start vertex (here we take the first vertex of the
-- graph)
isConnected :: UGraph -> Bool
isConnected gr = length (reachableVertices gr (vv!!0)) == length vv
    where vv = vertices gr

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

{--------------------------------------------------------------------------------
 -
 - Depth first search on graphs and related
 -
 - TODO: use mutable array for performance
 -
 -------------------------------------------------------------------------------}

type Visited = Array Vertex Bool

-- preorder of a B-tree
preorder :: Tree a -> [a]
preorder (Node x xs) = (x:(concatMap preorder xs))

-- postorder of a B-tree
postorder :: Tree a -> [a]
postorder (Node x xs) = concatMap postorder xs ++ [x]

-- create a tree from an UGraph given a root vertex. This tree then
-- contains all reachable vertices from the given vertex and is
-- infinte in size. It has to be filtered, such that every vertex
-- appears only once as a node, using depth-first search.
uGraphToTree :: Vertex -> UGraph -> Tree Vertex
uGraphToTree v gr = Node v (map (\x -> uGraphToTree x gr) (adjVertices v gr))

-- remove duplicate vertices in a forest of vertex trees
rmDuplVertices :: Forest Vertex -> State Visited (Forest Vertex)
rmDuplVertices [] = return []
rmDuplVertices ((Node v rest):frst) = do
    visited <- get
    case (visited!v) of
        False -> do modify (\x -> x // [(v,True)])
                    redRest <- rmDuplVertices rest
                    redFrst <- rmDuplVertices frst
                    return ((Node v redRest):redFrst)
        True  -> do rmDuplVertices frst

-- perform depth-first search on a graph
depthFirst :: UGraph -> [Vertex] -> Forest Vertex
depthFirst g v = filterForest bnds (map (flip uGraphToTree g) v) falseArr
    where filterForest bnds ts = fst . runState (rmDuplVertices ts)
          falseArr = listArray bnds $ repeat False
          bnds = vertexBounds g


{----------------------------------------------------------------------
 -
 - Graph generation (no multigraph support yet)
 -
 - Given a node degree partition D = {(N_1,d_1),..,(N_k,d_k)) with d_1
 - >= d_2 >= ..  for N_i nodes of degree d_i, the graph is constructed
 - by finding all d_j arcs emerging from a node n \in N_j. Nodes are
 - organized in equivalence classes EC = {(NE_1, d_1) ... (NE_c,d_c)}
 - that represent interconnections between the same vertices and arcs
 - are formed between the elements of EC. Initially, EC = D. During
 - construction, EC is refined such that after determining the next
 - d_j connections from a node n_j, any two nodes are found in the
 - same EC_i only if they were in the same class before and were both
 - (not) connected to n_j. The possible permutations of m connections
 - from a vertex of degree d_i=m to vertices in EC are called
 - m-sequences
 -
 - The resulting list of integrals contains both connected and
 - disconnected graphs. Moreover, it still contains isomorphic graphs
 - and has to be filter for a list of unique graphs.
 -
 - TODO: extend to multigraphs containing loops
 - TODO: improve by checking for canonic adjacency matrices during
 -       construction
 -
 ---------------------------------------------------------------------}

data EquivClass = EC { order :: Int, verts :: [Vertex] } deriving (Eq, Show)
-- we use simple lists for now, but other data types might be
-- beneficial for speed
type ECPartition = [EquivClass]
type MSequence = [Int]
-- TODO: eventually replace with UGraph
type AdjMat = [[Int]]
type AdjMatState a = State AdjMat a

-- number of vertices in an equivalence class
ecLength :: EquivClass -> Int
ecLength = length . verts

-- number of vertices in an ECPartition
ecPartitionVertices :: ECPartition -> Int
ecPartitionVertices = sum . (map ecLength)

-- compute the m-sequences for a given bound and a Partition
msequences :: Int -> ECPartition -> [MSequence]
msequences m x = boundSequences m (map ecLength x)

-- get an ECPartition for the given (degree, #vertices) pairs. Vertex
-- count starts at 0
initPartition :: [(Int, Int)] -> ECPartition
initPartition xxs = f 0 xxs
    where f n [] = []
          f n ((x,y):xs) = (EC x [n..(n+y-1)]):(f (n+y) xs)

-- split a single equivalence class for the given start node and
-- number of arcs
splitEC :: Int -> Int -> EquivClass -> AdjMatState ECPartition
splitEC n x ec
    | (ecLength ec - x == 0) && ((order ec) > 1) = ret [ec2]
    | (ecLength ec - x == 0) && ((order ec) == 1) = ret []
    | ecLength ec - x < 0 = error "reduceEC: illegal argument"
    | order ec > 1 && x /= 0 = ret [ec1,ec2]
    | otherwise = ret [ec1]
    where ec1 = EC (order ec) (drop x (verts ec))
          ec2 = EC (order ec - 1) (take x (verts ec))
          ret xs = do
              modify (appendAtNth n (take x (verts ec)))
              return xs


-- split a partition for a given start node and m-sequence
-- TODO: pass EC reduction method as argument
splitPartition :: Int -> MSequence -> ECPartition -> AdjMatState ECPartition
splitPartition _ _ []  = return ([])
splitPartition _ [] _  = return ([])
splitPartition start (s:ss) pp@(p:ps)  = do
    x <- splitEC start s p
    y <- splitPartition start ss ps
    return (x ++ y)


-- generate all possible adjacency matrices for the given partition starting from
-- the adjacency array (usually empty at the beginning)
-- TODO: simplify, use state monad
genDegGraphs :: (ECPartition, AdjMat)-> [(ECPartition, AdjMat)]
genDegGraphs ([],gr) = [([], gr)]
genDegGraphs (p, gr) | s /= [[]] = (concatMap genDegGraphs red)
                     | otherwise = []
    where (node, order, newp) = nextConnections p
          s = msequences order newp
          red = map (\x -> runState x gr) (splitForList node s newp)
          -- next node, number of arcs and the resulting partition for a given
          -- partition
          nextConnections :: ECPartition -> (Int, Int, ECPartition)
          nextConnections ((EC o nodes):xs) = case nodes of
              [n] -> (n, o, xs)
              (n:ns) -> (n, o, (EC o ns):xs)
          -- split a partition for a given start node and list of m-sequences
          splitForList :: Int -> [MSequence] -> ECPartition -> [AdjMatState ECPartition]
          splitForList start seq p = map (\x -> splitPartition start x p) seq

{----------------------------------------------------------------------
 -
 - Isomorphisms and automorphisms of graphs
 -
 - Ref: Practical Graph Isomorphism - B.D.McKak 1981
 -
 ---------------------------------------------------------------------}

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
isDiscrete = all (isTrivial)

isUnit :: Partition -> Bool
isUnit = (==) 1 . length

fix :: Partition -> [Vertex]
fix = concat . filter isTrivial

supp :: Partition -> [Vertex]
supp = concat . filter (not . isTrivial)

refineCell :: UGraph -> Cell -> Cell -> Partition -> (Partition, Partition)
refineCell gr c1 c2 p
    | isDiscrete xp = ([c1], p)
    | otherwise = (xp, neww ++ rest)
        where xp = splitCell gr c1 c2
              (maxX, rest) = splitLongest xp
              neww = replaceElem c1 maxX p

-- refine a single cell with respect to another one
splitCell :: UGraph -> Cell -> Cell -> Partition
splitCell gr c = groupSort (\x -> cellDegree gr c x)
    where -- number of vertices in a cell adjacent to a given vertex
          cellDegree :: UGraph -> Cell -> Vertex -> Int
          cellDegree gr c v = sum $ map fromEnum $ map (isNeighbour gr v) c

{----------------------------------------------------------------------
 -
 - Util
 -
 ---------------------------------------------------------------------}

-- for a given y = (y_1,...,y_n) and a bound m, find all vectors
-- x = (x_1,...,x_n) such that |x| = m and x_i <= y_i
--
-- TODO: Extend to a -> ([a] -> Bool) -> [a] -> [[a]] to and
-- filter/order according to the given predicate
boundSequences :: (Num a, Ord a, Enum a) => a -> [a] -> [[a]]
boundSequences m x | m <= sum x = (fByM . sequence . ranges) x
                   | otherwise = [[]]
    where fByM = filter (\x -> sum x == m)
          ranges = map (\x -> [0..x])


-- append a list `e` to the list at position `n` of `list`
appendAtNth :: (Show a) => Int -> [a] -> [[a]] -> [[a]]
appendAtNth n e list | length list > n = x ++ [(y++e)] ++ xs
                     | otherwise = error $ "appendAtNth: could not append"
    where (x,y:xs) = splitAt n list

-- sort and group a list by the given function
-- TODO: optimize
groupSort :: Ord k => (a -> k) -> [a] -> [[a]]
groupSort f x = map (map snd) $ groupBy fstEq $ sortBy fstCmp $ tpl f x
    where fstCmp x y = compare (fst x) (fst y)
          fstEq x y = fst x == fst y
          tpl f x = map (\y -> (f y, y)) x

-- replace an element in a list given a predicate
replaceIf :: (a -> Bool) -> a -> [a] -> [a]
replaceIf f n = map (\x -> if f x then n else x)

-- split the longest sublist of a list of lists and return it together
-- with the rest
-- TODO: simplify
splitLongest :: [[a]] -> ([a], [[a]])
splitLongest x = (longest, before ++ after)
    where maxlength = maximum $ map (length) x
          maxlIx = firstAtWith (\x -> length x == maxlength) x
          (before, (longest:after)) = splitAt maxlIx x

-- return the index to the first element in a list fulfilling the
-- predicate
firstAtWith:: (a -> Bool) -> [a] -> Int
firstAtWith f x = countUntil f x 0
    where countUntil f (x:xs) n | f x = n
                                | otherwise = countUntil f xs (n+1)

-- replace an element in a list with another one
replaceElem :: (Eq a) => a -> a -> [a] -> [a]
replaceElem _ _ [] = []
replaceElem old new (x:xs) | x == old = (new:xs)
                           | otherwise = [x] ++ replaceElem old new xs

