module Generate (degreeGraphs) where

import Graph
import Util
import Control.Monad.State
import Data.List

{----------------------------------------------------------------------
 -
 - Graph generation
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
 - and has to be filtered for a list of unique graphs.
 -
 - TODO: improve by checking for canonic adjacency matrices during
 -       construction
 -
 ---------------------------------------------------------------------}

{---------------------------------------------------------------------
 -
 - Public exports
 -
 ---------------------------------------------------------------------}

degreeGraphs :: [(Int,Int)] -> [UGraph]
degreeGraphs degreeSeq = map adjMatToUGraph $ map snd reduction
    where p = initPartition degreeSeq
          red =  genDegGraphs (p, initIAdj (ecPartitionVertices p))
          reduction =  filter (\x -> any (\y -> y /= []) (snd x)) $ red

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

initIAdj :: Int -> AdjMat
initIAdj n = replicate n []

-- temporary helper function
adjMatToUGraph :: AdjMat -> UGraph
adjMatToUGraph m = createUGraph (amEdges m)
  where amEdges m = [(fst x, y) | x <- zz, y <- snd x]
                    where zz = zip [0..] m
-- number of vertices in an equivalence class
ecLength :: EquivClass -> Int
ecLength = length . verts

-- number of vertices in an ECPartition
ecPartitionVertices :: ECPartition -> Int
ecPartitionVertices = sum . (map ecLength)

-- get an ECPartition for the given (degree, #vertices) pairs. Vertex
-- count starts at 0
initPartition :: [(Int, Int)] -> ECPartition
initPartition xxs = f 0 xxs
    where f n [] = []
          f n ((x,y):xs) = (EC x [n..(n+y-1)]):(f (n+y) xs)

type Count = Int
type Arcs = Int
type NodeSelection = [(Arcs,Count)]
type ECNodeSelection = (EquivClass, NodeSelection)

-- return m-sequences (combinations of number of arcs) for the given
-- order to an ECPartition
arcSeq :: Int -> ECPartition -> [MSequence]
arcSeq m x = boundSequences m (map ecProd x)
    where ecProd e = ecLength e * order e

-- return all the possible arc combinations  to an equivalence
-- class for a given number of arcs
connectionCombinations :: Int -> EquivClass -> [NodeSelection]
connectionCombinations arcs = map groupOcc . prune arcs . sequence . orderRep
    where orderRep (EC o v) = replicate (length v) [0..o]
          prune a = nub . map (reverse . sort) . filter ((== a) . sum)
          groupOcc = filter ((/= 0) . fst) . occurences

-- return all the possible lists of equivalence class node selections
-- from a Partition and a givne number of arcs
connections :: ECPartition -> Int -> [[ECNodeSelection]]
connections p order = concatMap (con p) $ arcSeq order p
     where con p arcs = map (zip p) $ sequence $ zipWith connectionCombinations arcs p

-- return all connections grouped by number of loops to the start
-- vertex
connectionsWLoops :: ECPartition -> Int -> [(Int, [[ECNodeSelection]])]
connectionsWLoops x m = filter prune $ map (conMap x m) [0,2..m]
    where prune s = case s of
                        (v,[[]]) -> if (v < m) then False else True
                        _        -> True
          conMap x m n = (n, connections x (m-n))

-- split an equivalence class and modify the adjacancy matrix state to
-- reflect the new arcs
splitEC :: Vertex -> ECNodeSelection -> AdjMatState ECPartition
splitEC vi ec = liftM (map fst) $ (splitAll ec)
    where splitAll :: ECNodeSelection -> AdjMatState [ECNodeSelection]
          splitAll (EC _ [], _) = return []
          splitAll ens@(ec, x) = case x of
              [] -> return [ens]
              (x:xs) -> do modify $ addarcs vi order $ take count (verts ec)
                           liftM2 (++) (return splitpart) $ splitAll next
                        where (order, count) = x
                              splitpart = splitSingle count order ec
                              next = (ecDrop count ec, xs)
          splitSingle :: Int -> Int -> EquivClass -> [ECNodeSelection]
          splitSingle c o e  | (order e) - o > 0 = [(ecTake c o e, [])]
                             | otherwise         = []

          ecDrop :: Int -> EquivClass -> EquivClass
          ecDrop i (EC o vs) = EC o (drop i vs)

          ecTake :: Int -> Int -> EquivClass -> EquivClass
          ecTake i oo (EC o vs) = EC (o - oo) (take i vs)

-- split a partition for a given start vertex, number of loops at the
-- vertex and a partition represented as ECNodeSelection
splitPartition :: Vertex -> Int -> [ECNodeSelection] -> AdjMatState ECPartition
splitPartition v 0 [] = return ([])
splitPartition v selfc s = case s of
    [] -> modify (addloops v selfc) >> return ([])
    (s:ss) -> do
        x <- splitEC v s
        y <- splitPartition v selfc ss
        return (x ++ y)

-- Add arcs from a given vertex to a list of vertices, order times
addarcs :: Vertex -> Int -> [Vertex] -> AdjMat -> AdjMat
addarcs v order vs = appendsortedAtNth v (concat $ replicate order vs)

-- Add loops to the given vertex
addloops :: Vertex -> Int -> AdjMat -> AdjMat
addloops v o = appendsortedAtNth v (selfcouplings v o)
    where selfcouplings v o = replicate (o `div` 2) v

-- generate all graphs
genDegGraphs :: (ECPartition, AdjMat)-> [(ECPartition, AdjMat)]
genDegGraphs ([],gr) = [([], gr)]
genDegGraphs (p, gr) | ss /= [] = concatMap genDegGraphs red
                     | otherwise = []
    where (node, order, newp) = nextConnections p
          ss = connectionsWLoops newp order
          red = concatMap (\z ->  map (\x -> runState x gr) (splitForList node z)) ss
          nextConnections :: ECPartition -> (Int, Int, ECPartition)
          nextConnections ((EC o nodes):xs) = case nodes of
              [n] -> (n, o, xs)
              (n:ns) -> (n, o, (EC o ns):xs)
          splitForList :: Int -> (Int, [[ECNodeSelection]]) -> [AdjMatState ECPartition]
          splitForList start (z, ecc) = map (\x -> splitPartition start z x) ecc
