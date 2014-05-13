module Grasp.Interpreter (
    grasp
    ) where


import Data.Graph.Inductive.Graph( Node, LNode, LEdge, (&) )
import qualified Data.Graph.Inductive.Graph as Graph
import Data.List
import Grasp.Types
import Grasp.Parser



type IP = [LNode String]




grasp :: GraspProgram -> IO ()
grasp g =
	let ips = map (:[]) (nodesWithName g "grasp:main")
	in interpret g ips



reachable :: GraspProgram -> [IP] -> [Node]
reachable g ips =
	let startNodes = nub . (map fst) $ (namedNodes g) ++ (concat ips)
	in reach g startNodes []



reach :: GraspProgram -> [Node] -> [Node] -> [Node]
reach _ [] f = f
reach g s@(x:xs) f =
	let f' = nub (x:f)
	    s' = nub (xs ++ (Graph.suc g x))
	    g' = Graph.delNode x g
	in reach g' s' f'



garbageCollect :: GraspProgram -> [IP] -> GraspProgram
garbageCollect g ips =
	let unreachable = (Graph.nodes g) \\ (reachable g ips)
	in Graph.delNodes unreachable g




interpret :: GraspProgram -> [IP] -> IO ()
interpret g ips = return ()

