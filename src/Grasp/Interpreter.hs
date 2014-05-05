module Grasp.Interpreter (
    grasp
    ) where


import Grasp.Types
import Grasp.Parser



type IP = [LNode String]




grasp :: GraspProgram -> IO ()
grasp g =
	let ips = map (:[]) (nodesWithName g "grasp:main")
	in interpret g ips



reachable :: GraspProgram -> [LNode String]
reachable g = []



garbageCollect :: GraspProgram -> GraspProgram
garbageCollect g = g



interpret :: GraspProgram -> [IP] -> IO ()
interpret g ips = return ()

