module Generate (degreeGraphs) where

import Graph
import Util
import Control.Monad.State

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

{---------------------------------------------------------------------
 -
 - Public exports
 -
 ---------------------------------------------------------------------}

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

{---------------------------------------------------------------------
 -
 - Internal
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

