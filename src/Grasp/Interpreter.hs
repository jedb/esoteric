module Grasp.Interpreter (
    grasp
    ) where


import System.Random
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
execute g (cur:rest) out = do
    (g', cur') <-
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

    execute g' rest (cur':out)



isInteger :: String -> Bool
isInteger x =
    let check = readMaybe x :: Maybe Int
    in if (isJust check) then True else False



isFloat :: String -> Bool
isFloat x =
    let check = readMaybe x :: Maybe Float
    in if (isJust check) then True else False



reLabel :: GraspProgram -> Node -> String -> GraspProgram
reLabel g n s =
    let (mc,d) = Graph.match n g
        c = fromJust mc
        c' = (\(w,x,y,z) -> (w,x,s,z)) $ c
    in if (isNothing mc) then g else c' & d



getByLabel :: String -> [LEdge String] -> [LEdge String]
getByLabel name = filter (\(_,_,x) -> x == name)



targetLabels :: GraspProgram -> [LEdge String] -> [String]
targetLabels g = map (\(_,x,_) -> fromJust (Graph.lab g x))

targetNodes :: [LEdge String] -> [Node]
targetNodes = map (\(_,x,_) -> x)

targetLNodes :: GraspProgram -> [LEdge String] -> [LNode String]
targetLNodes g = map (\(_,x,_) -> (x, fromJust (Graph.lab g x)) )



updateIP :: IP -> [LNode String] -> IO IP
updateIP _ [] = return []
updateIP ip next =
    getStdRandom (randomR (0,length next)) >>=
    (\x -> return ((next !! x):(tail ip)) )



setI :: GraspProgram -> IP -> IO (GraspProgram, IP)
setI g ip = do
    let edges = Graph.out g (fst . head $ ip)

        inL = targetLabels g (getByLabel "in" edges)
        outN = targetNodes (getByLabel "out" edges)
        nextLN = targetLNodes g (getByLabel "next" edges)
    
    g' <- case inL of
            [] -> return g
            _ -> (getStdRandom (randomR (0,length inL))) >>=
                (\x -> return (foldl' (\gr n -> reLabel gr n (inL !! x)) g outN) )
    
    ip' <- updateIP ip nextLN

    return (g',ip')



newI :: GraspProgram -> IP -> IO (GraspProgram, IP)
newI g ip = do
    let node = fst . head $ ip
        edges = Graph.out g node

        tailN = targetNodes (getByLabel "tail" edges)
        headN = targetNodes (getByLabel "head" edges)
        labelL = targetLabels g (getByLabel "label" edges)

    g' <- case (tailN, headN, labelL) of
            (x,_,_) | length x /= 1 -> error ("Instruction " ++ (show node) ++
                                              " should only have one tail argument")
            (_,y,_) | length y /= 1 -> error ("Instruction " ++ (show node) ++
                                              " should only have one head argument")
            (_,_,z) | length z /= 1 -> error ("Instruction " ++ (show node) ++
                                              " should only have one label argument")
            (_,_,z) | isFloat (head z) -> error ("Instruction " ++ (show node) ++
                                                 " should have non-numeric label argument")
            (x,y,z) -> return (Graph.insEdge (head x, head y, head z) g)

    ip' <- updateIP ip (targetLNodes g' (getByLabel "next" (Graph.out g' node)))

    return (g',ip')



delI :: GraspProgram -> IP -> IO (GraspProgram, IP)
delI g ip = do
    let node = fst . head $ ip
        edges = Graph.out g node

        tailN = targetNodes (getByLabel "tail" edges)
        headN = targetNodes (getByLabel "head" edges)
        labelL = targetLabels g (getByLabel "label" edges)

        edgesToDel = filter (\(x,y,z) -> x `elem` tailN &&
                                        (headN == [] || y `elem` headN) &&
                                        (labelL == [] || z `elem` labelL)) (Graph.labEdges g)

        g' = foldl' (\gr e -> Graph.delLEdge e gr) g edgesToDel

    ip' <- updateIP ip (targetLNodes g' (getByLabel "next" (Graph.out g' node)))

    return (g',ip')



pushI :: GraspProgram -> IP -> IO (GraspProgram, IP)
pushI g ip = return (g,ip)

popI :: GraspProgram -> IP -> IO (GraspProgram, IP)
popI g ip = return (g,ip)

pickI :: GraspProgram -> IP -> IO (GraspProgram, IP)
pickI g ip = return (g,ip)

callI :: GraspProgram -> IP -> IO (GraspProgram, IP)
callI g ip = return (g,ip)

retI :: GraspProgram -> IP -> IO (GraspProgram, IP)
retI g ip = return (g,ip)



addI :: GraspProgram -> IP -> IO (GraspProgram, IP)
addI = addmulI sum

mulI :: GraspProgram -> IP -> IO (GraspProgram, IP)
mulI = addmulI product

addmulI :: ([Float] -> Float) -> GraspProgram -> IP -> IO (GraspProgram, IP)
addmulI f g ip = do
    let node = fst . head $ ip
        edges = Graph.out g node

        argL = targetLabels g (getByLabel "arg" edges)
        outN = targetNodes (getByLabel "out" edges)
        nextLN = targetLNodes g (getByLabel "next" edges)

    g' <- case argL of
            x | not (all isFloat x) -> error ("Instruction " ++ (show node) ++
                                            " has non numeric arguments")
            x -> let s = f . map (read :: String -> Float) $ x
                 in return (foldl' (\gr n -> reLabel gr n (show s)) g outN)

    ip' <- updateIP ip nextLN

    return (g',ip')



subI :: GraspProgram -> IP -> IO (GraspProgram, IP)
subI g ip = return (g,ip)

divI :: GraspProgram -> IP -> IO (GraspProgram, IP)
divI g ip = return (g,ip)

modI :: GraspProgram -> IP -> IO (GraspProgram, IP)
modI g ip = return (g,ip)

getcI :: GraspProgram -> IP -> IO (GraspProgram, IP)
getcI g ip = return (g,ip)

putcI :: GraspProgram -> IP -> IO (GraspProgram, IP)
putcI g ip = return (g,ip)

getsI :: GraspProgram -> IP -> IO (GraspProgram, IP)
getsI g ip = return (g,ip)

putsI :: GraspProgram -> IP -> IO (GraspProgram, IP)
putsI g ip = return (g,ip)

implicitPushI :: GraspProgram -> IP -> IO (GraspProgram, IP)
implicitPushI g ip = return (g,ip)

