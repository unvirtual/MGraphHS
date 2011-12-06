module Generate (degreeGraphs) where

import Graph
import Util
import Control.Monad.State
import Data.List
import Data.Function (on)

import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad.ST

import Debug.Trace

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

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
 -       --> after each connection (in splitEC) check if the resulting
 -       new partition has a smaller indicator than the parent with
 -       respect to symmetry transformations of the parent
 -
 -       A connection is valid if
 -          * the max adjacency of the parent is larger than the adjacency
 -            of the child (disregarding self couplings)
 -          * if both max adjacencies are equal, then the the parent
 -            should have more higher degree connections as the child
 -            (disregarding self couplings)
 -          * if both the connection distribution and the max
 -            adjacency are the same
 -       The comparison only happens between nodes that are smaller
 -       than the parent node (already established connections in
 -       precious parent->child connections) and also are in the same
 -       permutation group. This means, that the first vertex of a
 -       given initial class doesn't have to be checked. Any next
 -       vertex is only checked against the vertices of the same
 -       group. This can be even further optimized if one keeps track
 -       of whether a vertex has reached the maximal degree already or
 -       not.
 -
 - TODO: discard degree partitions that connect external vertices
 -
 ---------------------------------------------------------------------}

{---------------------------------------------------------------------
 -
 - Public exports
 -
 ---------------------------------------------------------------------}

degreeGraphs :: [(Int,Int)] -> [Graph]
degreeGraphs degreeSeq = map (adjMatToGraph . snd) reduction
    where dsClean = sortBy (compare `on` snd)
                    $ filter (\(x,y) -> (x /= 0) && (y /= 0)) degreeSeq
          p = initPartition dsClean
          red =  trace (show dsClean ++ " initP: " ++ show p) genDegGraphs p (p, initAdj)
          initAdj = initIAdj (ecPartitionVertices p)
          --reduction =  filter (any (\y -> y /= []) . fst) red
          reduction = red


-- generate all graphs on `n` vertices with all possible combinations
-- of degrees `degrees`
allGraphs :: Int -> [Int] -> [Graph]
allGraphs nvert degrees = undefined

{---------------------------------------------------------------------
 -
 - Internal
 -
 ---------------------------------------------------------------------}
data EquivClass = NULL | EC { order :: Int, verts :: [Vertex] } deriving (Eq, Show)
-- we use simple lists for now, but other data types might be
-- beneficial for speed
type ECPartition = [EquivClass]
type MSequence = [Int]
-- TODO: eventually replace with Graph
type AdjMatState a = State AdjMatrix a

initIAdj :: Int -> AdjMatrix
initIAdj n = V.replicate n (UV.replicate n 0)

-- temporary helper function
adjMatToGraph :: AdjMatrix -> Graph
adjMatToGraph m = Graph (0,l) m
  where l = V.length m - 1

-- number of vertices in an equivalence class
ecLength :: EquivClass -> Int
ecLength = length . verts

-- number of vertices in an ECPartition
ecPartitionVertices :: ECPartition -> Int
ecPartitionVertices = sum . map ecLength

-- get an ECPartition for the given (degree, #vertices) pairs. Vertex
-- count starts at 0
initPartition :: [(Int, Int)] -> ECPartition
initPartition xxs = f 0 xxs
    where f n [] = []
          f n ((x,y):xs) = EC x [n..(n+y-1)]:f (n+y) xs

type Count = Int
type Arcs = Int
type NodeSelection = [(Arcs,Count)]
type ECNodeSelection = (EquivClass, NodeSelection)

-- return m-sequences (combinations of number of arcs) for the given
-- order to an ECPartition
arcSeq :: Int -> ECPartition -> [MSequence]
arcSeq m x = boundSequences m (map ecProd x)
    where ecProd e = ecLength e * order e

---- return all the possible arc combinations  to an equivalence
---- class for a given number of arcs
connectionCombinations :: Int -> EquivClass -> [NodeSelection]
connectionCombinations arcs = map groupOcc . nub . map (sortBy (flip compare)) .
                                     boundSequences arcs . orderRep
    where orderRep (EC o v) = replicate (length v) o
          groupOcc = filter ((/= 0) . fst) . occurences

-- return all the possible lists of equivalence class node selections
-- from a Partition and a givne number of arcs
connections :: ECPartition -> Int -> [[ECNodeSelection]]
connections p order = concatMap (con p) $ arcSeq order p
     where con p arcs = map (zip p) $ zipWithM connectionCombinations arcs p

-- return all connections grouped by number of loops to the start
-- vertex
connectionsWLoops :: ECPartition -> Int -> [(Int, [[ECNodeSelection]])]
connectionsWLoops x m = filter prune $ map (conMap x m) [0,2..m]
    where prune s = case s of
                        (v,[[]]) -> v >= m
                        _        -> True
          conMap x m n = (n, connections x (m-n))

---- split an equivalence class and modify the adjacancy matrix state to
---- reflect the new arcs
splitEC :: Vertex -> ECNodeSelection -> AdjMatState ECPartition
splitEC vi ec = liftM (map fst) $ splitAll ec
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
          splitSingle c o e  | order e - o > 0 = [(ecTake c o e, [])]
                             | otherwise         = []

          ecDrop :: Int -> EquivClass -> EquivClass
          ecDrop i (EC o vs) = EC o (drop i vs)

          ecTake :: Int -> Int -> EquivClass -> EquivClass
          ecTake i oo (EC o vs) = EC (o - oo) (take i vs)

-- split a partition for a given start vertex, number of loops at the
-- vertex and a partition represented as ECNodeSelection
splitPartition :: Vertex -> Int -> [ECNodeSelection] -> AdjMatState ECPartition
splitPartition v 0 [] = return []
splitPartition v selfc s = case s of
    [] -> modify (addloops v selfc) >> return []
    (s:ss) -> do
        x <- splitEC v s
        y <- splitPartition v selfc ss
        return (x ++ y)

-- Add arcs from a given vertex to a list of vertices, order times
addarcs :: Vertex -> Int -> [Vertex] -> AdjMatrix -> AdjMatrix
addarcs v order vs = rowupdate . colupdate
    where inds = map (flip (,) order) vs
          occs = occurences vs
          rowupdate m = (V.//) m [(v, UV.accum (+) ((V.!) m v) inds)]
          colupdate m = (V.//) m [(i, UV.accum (+) ((V.!) m i) (replicate order (v, j))) | (i,j) <- occs]

-- Add loops to the  given vertex
addloops :: Vertex -> Int -> AdjMatrix -> AdjMatrix
addloops v order m = (V.//) m [(v, UV.accum (+) ((V.!) m v) [(v, order)])]

pretty :: AdjMatrix -> [Char]
pretty = prettyCol . V.toList
    where prettyCol [] = []
          prettyCol (x:xs) = prettyRow (UV.toList x) ++ "\n" ++ prettyCol xs
          prettyRow [] = []
          prettyRow (x:xs) = show x ++ " " ++ prettyRow xs

---- generate all graphs
genDegGraphs :: ECPartition -> (ECPartition, AdjMatrix)-> [(ECPartition, AdjMatrix)]
genDegGraphs ip ([],gr) = [([], gr)]
genDegGraphs ip (p, gr) | ss /= [] = concatMap (genDegGraphs ip) red
                        | otherwise = []
    where (node, order, newp) = nextConnections p
          ss = connectionsWLoops newp order
          red = filter (\(x,y) -> x /= [NULL]) $ concatMap (map (`runState` gr) . splitForList node) ss
          nextConnections :: ECPartition -> (Int, Int, ECPartition)
          nextConnections (EC o nodes:xs) = case nodes of
              [n] -> (n, o, xs)
              (n:ns) -> (n, o, (EC o ns):xs)
          splitForList :: Int -> (Int, [[ECNodeSelection]]) -> [AdjMatState ECPartition]
          splitForList start (z, ecc) = sfl ecc ---map (splitPartition start z) ecc
               where sfl [] = []
                     sfl (ex:exs) = flip (:) (sfl exs) $ do
                                       part <- splitPartition start z ex
                                       mat <- get
                                       let parent = getParent node ip
                                       () <- return $ trace (show node ++ " " ++ show order ++ " " ++ show part) ()
                                       () <- return $ trace (pretty mat) ()
                                       if parent == node
                                            then do return part
                                            else do
                                                   let res = cmpVertices parent node parent mat
                                                   () <- return(trace ("p : " ++ show parent ++ " c: " ++ show node ++ " res: " ++ show res) ())
                                                   if res == LT then do return [NULL] else do return part

getParent :: Vertex -> ECPartition -> Vertex
getParent _ [] = error "wrong stuff"
getParent v (x:xs) | v `elem` (verts x) = head $ verts x
                   | otherwise          = getParent v xs

bucket :: Vertex -> Int -> AdjMatrix -> [(Int, Int)]
bucket v n = occurences . removeStuff v n . UV.toList . flip (V.!) v

removeElem :: Vertex -> [Int] -> [Int]
removeElem v l = (take v l) ++ (drop (v+1) l)

removeStuff :: Vertex -> Int -> [Int] -> [Int]
removeStuff v n l | v <= n    = drop (n + 1) l
                  | otherwise = drop n $ removeElem v l

maxConnectionOrder :: Vertex -> Int -> AdjMatrix -> Int
maxConnectionOrder v n = fst . head . bucket v n

cmpVertices :: Vertex ->  Vertex -> Int -> AdjMatrix -> Ordering
cmpVertices pv cv n mat | maxc pv /= maxc cv = trace (show (maxc pv) ++ " != " ++  show (maxc cv)) maxc pv `compare` maxc cv
                        | otherwise          = trace (show (bucket pv n mat) ++ " " ++ show (bucket cv n mat)) cmpBuckets (bucket pv n mat)
                                                          (bucket cv n mat)
    where maxc v = maxConnectionOrder v n mat

cmpBuckets :: [(Int, Int)] -> [(Int, Int)] -> Ordering
cmpBuckets [] [] = EQ
cmpBuckets xx@((x,nx):xs) yy@((y,ny):ys) | x /= y = trace ("x /= y") x `compare` y
                                         | nx /= ny = trace ("nx /= ny") nx `compare` ny
                                         | otherwise = cmpBuckets xs ys
