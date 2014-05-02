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




type GraspProgram = Gr String String



instance (Ord a, Ord b) => Eq (Gr a b) where
	a == b  =   ((sort . Graph.labNodes $ a) == (sort . Graph.labNodes $ b)) &&
	            ((sort . Graph.labEdges $ a) == (sort . Graph.labEdges $ b))




nodesWithName :: GraspProgram -> String -> [LNode String]
nodesWithName g s = []



normalise :: GraspProgram -> GraspProgram
normalise g = Graph.mkGraph [] []



iso :: GraspProgram -> GraspProgram -> Bool
iso a b = (normalise a) == (normalise b)

