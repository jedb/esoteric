module Grasp.Types (
	GraspProgram(..),

	nodesWithName,
	normalise,
	iso
    ) where


import Data.Graph.Inductive.Graph( Node, LNode, LEdge, (&) )
import qualified Data.Graph.Inductive.Graph as Graph
import Data.Graph.Inductive.Tree
import Data.List
import Data.Maybe
import qualified Data.Map as Map




type GraspProgram = Gr String String



instance (Ord a, Ord b) => Eq (Gr a b) where
	a == b  =   ((sort . Graph.labNodes $ a) == (sort . Graph.labNodes $ b)) &&
	            ((sort . Graph.labEdges $ a) == (sort . Graph.labEdges $ b))




nodesWithName :: GraspProgram -> String -> [LNode String]
nodesWithName g s =
	let nodes = Graph.labNodes g
	    edges = Graph.labEdges g

	    nodeLabelMap = Map.fromList nodes

	    nameEdges = filter (\(_,_,z) -> z == "name") edges
	    specific = filter (\(_,y,_) -> fromJust (Map.lookup y nodeLabelMap) == s) nameEdges
	    nameNodes = map (\(x,_,_) -> x) specific

	in filter (\(x,_) -> x `elem` nameNodes) nodes



-- to-do
normalise :: GraspProgram -> GraspProgram
normalise g = Graph.mkGraph [] []



iso :: GraspProgram -> GraspProgram -> Bool
iso a b = (normalise a) == (normalise b)

