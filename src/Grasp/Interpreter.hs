module Grasp.Interpreter (
    grasp
    ) where


import Data.Graph.Inductive.Graph( Node, LNode, LEdge, (&) )
import qualified Data.Graph.Inductive.Graph as Graph
import qualified Data.Set as Set
import Grasp.Types
import Grasp.Parser



type IP = [LNode String]




grasp :: GraspProgram -> IO ()
grasp g =
	let ips = map (:[]) (nodesWithName g "grasp:main")
	in interpret g ips



reachable :: GraspProgram -> [IP] -> [Node]
reachable g ips = Graph.nodes g



garbageCollect :: GraspProgram -> [IP] -> GraspProgram
garbageCollect g ips =
	let unreachable = (Graph.nodes g) \\ (reachable g ips)
	in Graph.delNodes unreachable g




interpret :: GraspProgram -> [IP] -> IO ()
interpret g ips = return ()

