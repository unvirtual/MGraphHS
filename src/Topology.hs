module Topology where

{----------------------------------------------------------------------
 -
 - A topology is a directed multigraph with edges labelled by momenta
 -
 ----------------------------------------------------------------------}

import Data.List
import MultiGraph
import MultiGraph.Morphisms
import Momentum
import Algebra
import LinearSolve
import Data.Graph (Graph, buildG)
import Data.Ratio

import qualified Data.Vector.Unboxed as UV

type GraphMomentum = Momentum Int MomentumLabel
type RatioMomentum = Momentum (Ratio Int) MomentumLabel

data DiGraph  = DiGraph MultiGraph [Edge] deriving Show
data Topology = Topology (MultiGraph, [(Edge, GraphMomentum)]) deriving Show

digraph :: MultiGraph -> DiGraph
digraph g = DiGraph g (edges g)

--sortEdges :: DiGraph -> DiGraph
sortEdges (DiGraph g e) = DiGraph g flippedEdges
    where externalVertices = externalNodes g
          isExternalEdge (v0,v1) =
                v0 `elem` externalVertices || v1 `elem` externalVertices
          orderEdges e1 e2 | isExternalEdge e1 = LT
                           | isExternalEdge e2 = GT
                           | otherwise = EQ
          extIncoming (v0,v1) | v1 `elem` externalVertices = (v1,v0)
                              | otherwise = (v0,v1)
          sortedEdges = sortBy orderEdges e
          flippedEdges = map extIncoming sortedEdges

topologyFromMultiGraph :: MultiGraph -> Topology
topologyFromMultiGraph g = Topology (gg, zip ee mm)
    where (DiGraph gg ee) = sortEdges $ digraph g
          momenta = moms (length $ externalNodes g)
          nedges = length (edges g) - length momenta
          mm = momenta ++ map (\x -> M [(X x, 1)]) [1..nedges]

moms :: Int -> [GraphMomentum]
moms legs = extBasis
    where indepExt = map (\x -> M [(P x, 1)]) [1..legs-1]
          extBasis = indepExt ++ [Algebra.neg $ Algebra.sum indepExt]

edgesPerNode :: Topology -> [[(Edge, GraphMomentum)]]
edgesPerNode (Topology (g, ee)) = map (nodeEdges ee) inodes
    where inodes = init $ internalNodes g
          nodeEdges ee n = map (\x -> flipMomentum n x) $
                           filter (\x -> nodeInEdge n (fst x)) ee
          nodeInEdge n e = n == fst e || n == snd e
          flipMomentum n (e,m) | n == fst e && (snd e) `elem` (externalNodes g) = (e,m)
                               | n == snd e = (e,m)
                               | n == fst e = (e, Algebra.neg m)

nodeEqn :: [(Edge, GraphMomentum)] -> Int -> Int -> ([Int], GraphMomentum)
nodeEqn ee n l = (map snd $ toList $ Algebra.neg $ project undefBasis momsum, simplify $ project defBasis momsum)
    where undefBasis = Algebra.sum $ map (\x -> M [(X x, 1)]) [1..n]
          defBasis = Algebra.sum $ init $ moms l
          momsum = Algebra.sum $ map (snd) ee

system :: [[(Edge, GraphMomentum)]] -> Int -> Int -> ([[Int]], [GraphMomentum])
system ee n l = foldr (\(x,y) (a,b) -> (a++[x], b ++ [y])) ([],[]) mapp
    where mapp = map (\x -> nodeEqn x n l) ee

ratioSystem :: ([[Int]], [GraphMomentum]) -> ([[Ratio Int]], [RatioMomentum])
ratioSystem (x,y) = (map (map (flip (%) 1)) x, map momentumToRatioCoeffs y)
