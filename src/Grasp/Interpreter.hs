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
interpret g ips = if (ips == []) then return () else execute g ips []



execute :: GraspProgram -> [IP] -> [IP] -> IO ()
execute g [] out = interpret g (reverse out)
execute g ([]:ips) out = execute g ips out
execute g (cur:rest) out =
	let (g', cur') =
		    case (snd . head $ cur) of
		        "set" -> setI g cur
		        "new" -> newI g cur
		        "del" -> delI g cur
		        "push" -> pushI g cur
		        "pop" -> popI g cur
		        "pick" -> pickI g cur
		        "call" -> callI g cur
		        "ret" -> retI g cur
		        "add" -> addI g cur
		        "mul" -> mulI g cur
		        "sub" -> subI g cur
		        "div" -> divI g cur
		        "mod" -> modI g cur
		        "getc" -> getcI g cur
		        "putc" -> putcI g cur
		        "gets" -> getsI g cur
		        "puts" -> puts g cur

		        x | isInteger x -> implicitPushI g cur

		        x -> error ("Unknown instruction at " ++ (show x))

    in execute g' rest (cur':out)



isInteger :: String -> Bool

setI :: GraspProgram -> IP -> (GraspProgram, IP)
newI :: GraspProgram -> IP -> (GraspProgram, IP)
delI :: GraspProgram -> IP -> (GraspProgram, IP)
pushI :: GraspProgram -> IP -> (GraspProgram, IP)
popI :: GraspProgram -> IP -> (GraspProgram, IP)
pickI :: GraspProgram -> IP -> (GraspProgram, IP)
callI :: GraspProgram -> IP -> (GraspProgram, IP)
retI :: GraspProgram -> IP -> (GraspProgram, IP)
addI :: GraspProgram -> IP -> (GraspProgram, IP)
mulI :: GraspProgram -> IP -> (GraspProgram, IP)
subI :: GraspProgram -> IP -> (GraspProgram, IP)
divI :: GraspProgram -> IP -> (GraspProgram, IP)
modI :: GraspProgram -> IP -> (GraspProgram, IP)
getcI :: GraspProgram -> IP -> (GraspProgram, IP)
putcI :: GraspProgram -> IP -> (GraspProgram, IP)
getsI :: GraspProgram -> IP -> (GraspProgram, IP)
putsI :: GraspProgram -> IP -> (GraspProgram, IP)

implicitPushI :: GraspProgram -> IP -> (GraspProgram, IP)

