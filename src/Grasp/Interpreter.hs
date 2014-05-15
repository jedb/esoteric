module Grasp.Interpreter (
    grasp
    ) where


import Text.Read( readMaybe )
import Data.Graph.Inductive.Graph( Node, LNode, LEdge, (&) )
import qualified Data.Graph.Inductive.Graph as Graph
import Data.List
import Data.Maybe
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
		        "puts" -> putsI g cur

		        x | isInteger x -> implicitPushI g cur

		        x -> error ("Unknown instruction at " ++ (show x))

    in execute g' rest (cur':out)



isInteger :: String -> Bool
isInteger x =
	let check = readMaybe x :: Maybe Int
	in if (isJust check) then True else False



reLabel :: GraspProgram -> Node -> String -> GraspProgram
reLabel g n s =
	let (mc,d) = Graph.match n g
	    c = fromJust mc
	    c' = (\(w,x,y,z) -> (w,x,s,z)) $ c
	in if (isNothing mc) then g else c' & d



setI :: GraspProgram -> IP -> (GraspProgram, IP)
setI g ip = (g,ip)

newI :: GraspProgram -> IP -> (GraspProgram, IP)
newI g ip = (g,ip)

delI :: GraspProgram -> IP -> (GraspProgram, IP)
delI g ip = (g,ip)

pushI :: GraspProgram -> IP -> (GraspProgram, IP)
pushI g ip = (g,ip)

popI :: GraspProgram -> IP -> (GraspProgram, IP)
popI g ip = (g,ip)

pickI :: GraspProgram -> IP -> (GraspProgram, IP)
pickI g ip = (g,ip)

callI :: GraspProgram -> IP -> (GraspProgram, IP)
callI g ip = (g,ip)

retI :: GraspProgram -> IP -> (GraspProgram, IP)
retI g ip = (g,ip)

addI :: GraspProgram -> IP -> (GraspProgram, IP)
addI g ip = (g,ip)

mulI :: GraspProgram -> IP -> (GraspProgram, IP)
mulI g ip = (g,ip)

subI :: GraspProgram -> IP -> (GraspProgram, IP)
subI g ip = (g,ip)

divI :: GraspProgram -> IP -> (GraspProgram, IP)
divI g ip = (g,ip)

modI :: GraspProgram -> IP -> (GraspProgram, IP)
modI g ip = (g,ip)

getcI :: GraspProgram -> IP -> (GraspProgram, IP)
getcI g ip = (g,ip)

putcI :: GraspProgram -> IP -> (GraspProgram, IP)
putcI g ip = (g,ip)

getsI :: GraspProgram -> IP -> (GraspProgram, IP)
getsI g ip = (g,ip)

putsI :: GraspProgram -> IP -> (GraspProgram, IP)
putsI g ip = (g,ip)

implicitPushI :: GraspProgram -> IP -> (GraspProgram, IP)
implicitPushI g ip = (g,ip)

