module Grasp.Interpreter (
    grasp
    ) where


import Control.Monad
import Control.Exception
import System.Random
import System.IO
import System.IO.Error
import Text.Read( readMaybe )
import Data.Graph.Inductive.Graph( Node, LNode, LEdge, (&) )
import qualified Data.Graph.Inductive.Graph as Graph
import Data.List
import Data.Maybe
import Data.Char
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
    let (node,instruction) = head cur
        condL = targetLabels g (getByLabel "cond" (Graph.out g node))
        goAhead = all (\x -> isFloat x && (read :: String -> Float) x /= 0.0) condL

    in case (goAhead, instruction) of
        (False,_) -> do
            cur' <- updateIP cur (targetLNodes g (getByLabel "next" (Graph.out g node)))
            execute g rest (cur':out)

        (True,x) | x /= "call" && x /= "ret" -> do
            g' <- case x of
                    "set" -> setI g node
                    "new" -> newI g node
                    "del" -> delI g node
                    "push" -> pushI g node
                    "pop" -> popI g node
                    "pick" -> pickI g node
                    "add" -> addI g node
                    "mul" -> mulI g node
                    "sub" -> subI g node
                    "div" -> divI g node
                    "mod" -> modI g node
                    "getc" -> getcI g node
                    "putc" -> putcI g node
                    "gets" -> getsI g node
                    "puts" -> putsI g node
                    inst | isInteger inst -> implicitPushI g node
                    _ -> error ("Unknown instruction at " ++ (show x))
            cur' <- updateIP cur (targetLNodes g' (getByLabel "next" (Graph.out g' node)))
            execute g' rest (cur':out)

        (_,x) -> do
            (g',cur') <- case x of
                    "call" -> callI g cur
                    "ret" -> retI g cur
                    _ -> error ("Execute function reached impossible branch")
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
    getStdRandom (randomR (0,length next - 1)) >>=
    (\x -> return ((next !! x):(tail ip)) )



setI :: GraspProgram -> Node -> IO GraspProgram
setI g node =
    let edges = Graph.out g node

        inL = targetLabels g (getByLabel "in" edges)
        outN = targetNodes (getByLabel "out" edges)

    in case inL of
        [] -> return g
        _ -> (getStdRandom (randomR (0,length inL - 1))) >>=
            (\x -> return (foldl' (\gr n -> reLabel gr n (inL !! x)) g outN) )



newI :: GraspProgram -> Node -> IO GraspProgram
newI g node =
    let edges = Graph.out g node

        tailN = targetNodes (getByLabel "tail" edges)
        headN = targetNodes (getByLabel "head" edges)
        labelL = targetLabels g (getByLabel "label" edges)

    in case (tailN, headN, labelL) of
        (x,_,_) | length x /= 1 -> error ("Instruction " ++ (show node) ++
                                            " should only have one tail argument")
        (_,y,_) | length y /= 1 -> error ("Instruction " ++ (show node) ++
                                            " should only have one head argument")
        (_,_,z) | length z /= 1 -> error ("Instruction " ++ (show node) ++
                                            " should only have one label argument")
        (_,_,z) | isFloat (head z) -> error ("Instruction " ++ (show node) ++
                                            " should have non-numeric label argument")
        (x,y,z) -> return (Graph.insEdge (head x, head y, head z) g)



delI :: GraspProgram -> Node -> IO GraspProgram
delI g node =
    let edges = Graph.out g node

        tailN = targetNodes (getByLabel "tail" edges)
        headN = targetNodes (getByLabel "head" edges)
        labelL = targetLabels g (getByLabel "label" edges)

        edgesToDel = filter (\(x,y,z) -> x `elem` tailN &&
                                        (headN == [] || y `elem` headN) &&
                                        (labelL == [] || z `elem` labelL)) (Graph.labEdges g)

    in return (foldl' (flip Graph.delLEdge) g edgesToDel)



pushI :: GraspProgram -> Node -> IO GraspProgram
pushI g node = do
    let edges = Graph.out g node

        stackN = targetNodes (getByLabel "stack" edges)
        inL = targetLabels g (getByLabel "in" edges)

    rnd <- getStdRandom (randomR (0,length inL -1))
    let label = if (inL == []) then "" else inL !! rnd

    if (length stackN /= 1)
    then error ("Instruction " ++ (show node) ++ " should only have one stack argument")
    else doPushI g (head stackN) label



implicitPushI :: GraspProgram -> Node -> IO GraspProgram
implicitPushI g node =
    let edges = Graph.out g node

        stackN = targetNodes (getByLabel "stack" edges)
        label = fromJust $ Graph.lab g node

    in if (length stackN /= 1)
        then error ("Instruction " ++ (show node) ++ " should only have one stack argument")
        else doPushI g (head stackN) label



doPushI :: GraspProgram -> Node -> String -> IO GraspProgram
doPushI g s l =
    let newN = head (Graph.newNodes 1 g)

        edgesToDel = Graph.inn g s
        edgesToAdd = map (\(x,y,z) -> (x,newN,z)) edgesToDel

        nextE = (newN,s,"next")

        g' = Graph.insNode (newN,l) g
        g'' = foldl' (flip Graph.delLEdge) g' edgesToDel
        g''' = Graph.insEdges (nextE:edgesToAdd) g''

    in return g'''



popI :: GraspProgram -> Node -> IO GraspProgram
popI g node = return g

pickI :: GraspProgram -> Node -> IO GraspProgram
pickI g node = return g



callI :: GraspProgram -> IP -> IO (GraspProgram, IP)
callI g ip = return (g,ip)

retI :: GraspProgram -> IP -> IO (GraspProgram, IP)
retI g ip = return (g,ip)



addI :: GraspProgram -> Node -> IO GraspProgram
addI = addmulI sum

mulI :: GraspProgram -> Node -> IO GraspProgram
mulI = addmulI product

addmulI :: ([Float] -> Float) -> GraspProgram -> Node -> IO GraspProgram
addmulI f g node =
    let edges = Graph.out g node

        argL = targetLabels g (getByLabel "arg" edges)
        outN = targetNodes (getByLabel "out" edges)

    in case argL of
        x | not (all isFloat x) -> error ("Instruction " ++ (show node) ++
                                        " has non numeric arguments")
        x -> let s = f . (map read) $ x
             in return (foldl' (\gr n -> reLabel gr n (show s)) g outN)



subI :: GraspProgram -> Node -> IO GraspProgram
subI = subdivI (\a b -> a - (sum b))

divI :: GraspProgram -> Node -> IO GraspProgram
divI = subdivI (\a b -> a / (product b))

subdivI :: (Float -> [Float] -> Float) -> GraspProgram -> Node -> IO GraspProgram
subdivI f g node =
    let edges = Graph.out g node

        leftL = targetLabels g (getByLabel "left" edges)
        rightL = targetLabels g (getByLabel "right" edges)
        outN = targetNodes (getByLabel "out" edges)

    in case (leftL, rightL) of
        (x,_) | length x /= 1 -> error ("Instruction " ++ (show node) ++
                                        " lacks a left edge")
        (x,y) | not (all isFloat x && all isFloat y) ->
                                error ("Instruction " ++ (show node) ++
                                        " has non numeric arguments")
        (x,y) -> let s = f (read . head $ x) (map read y)
                 in return (foldl' (\gr n -> reLabel gr n (show s)) g outN)



modI :: GraspProgram -> Node -> IO GraspProgram
modI g node =
    let edges = Graph.out g node

        leftL = targetLabels g (getByLabel "left" edges)
        rightL = targetLabels g (getByLabel "right" edges)
        outN = targetNodes (getByLabel "out" edges)

    in case (leftL, rightL) of
        (x,y) | length x /= 1 || length y /= 1 ->
                        error ("Instruction " ++ (show node) ++ " requires " ++
                                "a single left edge and a single right edge")

        (x,y) | not (all isInteger x && all isInteger y) ->
                        error ("Instruction " ++ (show node) ++
                                " has non integer arguments")

        (x,y) -> let s = (read . head $ x) `mod` (read . head $ y)
                 in return (foldl' (\gr n -> reLabel gr n (show s)) g outN)



getcI :: GraspProgram -> Node -> IO GraspProgram
getcI g node = do
    let edges = Graph.out g node

        outN = targetNodes (getByLabel "out" edges)
        fhL = targetLabels g (getByLabel "fh" edges)

    c <- case fhL of
            x | length x == 0 -> getChar >>=
                (\x -> if (x == '\EOT') then return (-1) else return (ord x))

            x | length x == 1 -> do
                    h <- openFile (head fhL) ReadMode
                    input <- try (hGetChar h)
                    hClose h
                    case input of
                        Left e -> if (isEOFError e) then return (-1) else ioError e
                        Right inpChr -> return (ord inpChr)

            x -> error ("Instruction " ++ (show node) ++
                        " may only have one file handle")

    return (foldl' (\gr n -> reLabel gr n (show c)) g outN)



putcI :: GraspProgram -> Node -> IO GraspProgram
putcI g node = do
    let edges = Graph.out g node

        inL = targetLabels g (getByLabel "in" edges)
        fhL = targetLabels g (getByLabel "fh" edges)

    r <- getStdRandom (randomR (0, length inL - 1))

    c <- case inL of
            x | length x == 0 ->
                        error ("Instruction " ++ (show node) ++
                                " must have at least one in edge")

            x | not (isInteger $ inL!!r) ->
                        error ("Randomly chosen in edge to " ++ (show node) ++
                                " does not contain an integer")

            x -> return . chr . read $ inL!!r

    fh <- case fhL of
            x | length x == 0 -> return stdout

            x | length x == 1 -> openFile (head fhL) AppendMode

            x -> error ("Instruction " ++ (show node) ++
                        " may only have one file handle")

    hPutChar fh c
    when (fh /= stdout) (hClose fh)

    return g



getsI :: GraspProgram -> Node -> IO GraspProgram
getsI g node = return g

putsI :: GraspProgram -> Node -> IO GraspProgram
putsI g node = return g

