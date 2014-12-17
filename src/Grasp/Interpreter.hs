module Grasp.Interpreter (
    grasp
    ) where




import System.IO( Handle, FilePath, IOMode )
import qualified System.IO as IO

import qualified System.Random as Random

import qualified Control.Monad as Monad

import Control.Monad.IO.Class( liftIO )

import qualified Data.Maybe as Maybe

import Data.List( (!!) )
import qualified Data.List as List

import qualified Data.Char as Char

import Grasp.Monad( GraspM )
import qualified Grasp.Monad as GMonad

import Grasp.Types.GNode( GNode )
import qualified Grasp.Types.GNode as GN

import Grasp.Types.GEdge( GEdge )
import qualified Grasp.Types.GEdge as GE

import Grasp.Types.Instruction( Instruction )
import qualified Grasp.Types.Instruction as IN

import Grasp.Types.EdgeLabel( EdgeLabel )
import qualified Grasp.Types.EdgeLabel as EL




grasp :: ([GNode],[GEdge]) -> IO ([GNode],[GEdge])
grasp input = GMonad.finalise $ GMonad.construct input >> interpret



interpret :: GraspM ()
interpret = do
    n <- GMonad.peekIP

    if (Maybe.isNothing n) then return () else do
        case (IN.toString . GN.toInst . Maybe.fromJust $ n) of
            "set" -> setI
            "new" -> newI
            "del" -> delI
            "push" -> pushI
            "pop" -> popI
            "pick" -> pickI
            "call" -> callI
            "ret" -> retI
            "add" -> addI
            "mul" -> mulI
            "sub" -> subI
            "div" -> divI
            "mod" -> modI
            "getc" -> getcI
            "putc" -> putcI
            "gets" -> getsI
            "puts" -> putsI
            inst | Maybe.isJust . IN.toInt . IN.mk $ inst -> implicitPushI
            x -> error ("Unknown instruction " ++ x)

        GMonad.nextIP
        interpret



setI :: GraspM ()
setI = do
    curNode <- GMonad.peekIP >>= return . Maybe.fromJust

    inNodes <- GMonad.nodesOut (EL.mk "in") curNode
    outNodes <- GMonad.nodesOut (EL.mk "out") curNode

    Monad.when (length inNodes /= 0) (do
        r <- liftIO (Random.getStdRandom (Random.randomR (0, length inNodes - 1)))
        mapM_ (GMonad.reLabel (GN.toInst (inNodes !! r))) outNodes )

    GMonad.updateIP



newI :: GraspM ()
newI = do
    curNode <- GMonad.peekIP >>= return . Maybe.fromJust

    tailNodes <- GMonad.nodesOut (EL.mk "tail") curNode
    headNodes <- GMonad.nodesOut (EL.mk "head") curNode
    labelNodes <- GMonad.nodesOut (EL.mk "label") curNode

    Monad.when (length tailNodes /= 1) (error "Instruction new should have one tail argument")
    Monad.when (length headNodes /= 1) (error "Instruction new should have one head argument")
    Monad.when (length labelNodes /= 1) (error "Instruction new should have one label argument")
    Monad.when (Maybe.isJust . IN.toFloat . GN.toInst $ (head labelNodes))
        (error "Label argument to instruction new should not be a number")

    let edgeLabel = EL.mk . IN.toString . GN.toInst $ (head labelNodes)
    GMonad.insEdge (GE.mk (GN.toNode (head tailNodes), GN.toNode (head headNodes), edgeLabel))

    GMonad.updateIP



delI :: GraspM ()
delI = do
    curNode <- GMonad.peekIP >>= return . Maybe.fromJust

    tailNodes <- GMonad.nodesOut (EL.mk "tail") curNode
    headNodes <- GMonad.nodesOut (EL.mk "head") curNode
    labelNodes <- GMonad.nodesOut (EL.mk "label") curNode

    Monad.when (length tailNodes /= 0) (do
        input <- mapM GMonad.edgesOut tailNodes >>= return . concat
        let labels = map (EL.mk . IN.toString . GN.toInst) labelNodes
            heads = map GN.toNode headNodes
            result = filter (\x -> (length headNodes == 0 || GE.toDest x `elem` heads) &&
                                    (length labelNodes == 0 || GE.toLabel x `elem` labels)) input
        GMonad.delEdges result )

    GMonad.updateIP



pushI :: GraspM ()
pushI = do
    curNode <- GMonad.peekIP >>= return . Maybe.fromJust

    stackNodes <- GMonad.nodesOut (EL.mk "stack") curNode
    inNodes <- GMonad.nodesOut (EL.mk "in") curNode

    Monad.when (length stackNodes /= 1) (error "Instruction push should have one stack argument")

    r <- liftIO (Random.getStdRandom (Random.randomR (0, length inNodes - 1)))
    let stackInst = if (length inNodes == 0) then IN.mk "" else GN.toInst (inNodes !! r)

    n <- GMonad.newNodes [stackInst]
    ne <- return (GE.mk (GN.toNode (head n), GN.toNode (head stackNodes), EL.mk "next"))
    se <- GMonad.edgesIn (head stackNodes)
    se' <- return (map (\x -> GE.mk (GE.toSrc x, GN.toNode (head n), GE.toLabel x)) se)

    GMonad.delEdges se
    GMonad.insNode (head n)
    GMonad.insEdges (ne:se')

    GMonad.updateIP



implicitPushI :: GraspM ()
implicitPushI = do
    curNode <- GMonad.peekIP >>= return . Maybe.fromJust

    stackNodes <- GMonad.nodesOut (EL.mk "stack") curNode

    Monad.when (length stackNodes /= 1) (error "Instruction implicit push should have one stack argument")

    n <- GMonad.newNodes [GN.toInst curNode]
    ne <- return (GE.mk (GN.toNode (head n), GN.toNode (head stackNodes), EL.mk "next"))
    se <- GMonad.edgesIn (head stackNodes)
    se' <- return (map (\x -> GE.mk (GE.toSrc x, GN.toNode (head n), GE.toLabel x)) se)

    GMonad.delEdges se
    GMonad.insNode (head n)
    GMonad.insEdges (ne:se')

    GMonad.updateIP



popI :: GraspM ()
popI = do
    curNode <- GMonad.peekIP >>= return . Maybe.fromJust

    stackNodes <- GMonad.nodesOut (EL.mk "stack") curNode
    outNodes <- GMonad.nodesOut (EL.mk "out") curNode
    emptyNodes <- GMonad.nodesOut (EL.mk "empty") curNode

    Monad.when (length stackNodes /= 1) (error "Instruction pop should have one stack argument")

    nextNodes <- GMonad.nodesOut (EL.mk "next") (head stackNodes)

    Monad.when (length nextNodes > 1) (error "Stack nodes should have at most one next edge")

    if (length nextNodes == 1) then do
        mapM_ (GMonad.reLabel (GN.toInst (head stackNodes))) outNodes

        se <- GMonad.edgesIn (head stackNodes)
        se' <- return (map (\x -> GE.mk (GE.toSrc x, GN.toNode (head nextNodes), GE.toLabel x)) se)

        GMonad.delEdges se
        GMonad.insEdges se'
    else do
        Monad.when (length emptyNodes > 0) (do
            r <- liftIO (Random.getStdRandom (Random.randomR (0, length emptyNodes - 1)))
            mapM_ (GMonad.reLabel (GN.toInst (emptyNodes !! r))) outNodes )

    GMonad.updateIP



pickI :: GraspM ()
pickI = do
    curNode <- GMonad.peekIP >>= return . Maybe.fromJust

    stackNodes <- GMonad.nodesOut (EL.mk "stack") curNode
    depthNodes <- GMonad.nodesOut (EL.mk "depth") curNode
    outNodes <- GMonad.nodesOut (EL.mk "out") curNode
    emptyNodes <- GMonad.nodesOut (EL.mk "empty") curNode

    Monad.when (length stackNodes /= 1) (error "Instruction pick should have one stack argument")
    Monad.when (length depthNodes /= 1) (error "Instruction pick should have one depth argument")
    Monad.unless (Maybe.isJust . IN.toInt . GN.toInst $ (head depthNodes))
        (error "Depth argument to instruction pick should be an integer")
    Monad.unless ((>=0) . Maybe.fromJust . IN.toInt . GN.toInst $ (head depthNodes))
        (error "Depth argument to instruction pick should be positive")

    let depthFunc d n = do
        nextNodes <- GMonad.nodesOut (EL.mk "next") n
        Monad.when (length nextNodes > 1) (error "Stack nodes should have at most one next edge")
        if (length nextNodes == 0)
            then return Nothing
            else if (d == 0)
                then return (Just n)
                else depthFunc (d - 1) (head nextNodes)

    pickNode <- depthFunc (Maybe.fromJust . IN.toInt . GN.toInst $ (head depthNodes)) (head stackNodes)

    if (Maybe.isJust pickNode) then do
        mapM_ (GMonad.reLabel (GN.toInst (Maybe.fromJust pickNode))) outNodes
    else do
        Monad.when (length emptyNodes > 0) (do
            r <- liftIO (Random.getStdRandom (Random.randomR (0, length emptyNodes - 1)))
            mapM_ (GMonad.reLabel (GN.toInst (emptyNodes !! r))) outNodes )

    GMonad.updateIP



callI :: GraspM ()
callI = do
    curNode <- GMonad.peekIP >>= return . Maybe.fromJust

    funcNodes <- GMonad.nodesOut (EL.mk "func") curNode
    argEdges <- GMonad.edgesOut curNode
    ae <- return (filter (\x -> (GE.toLabel x) /= (EL.mk "name") && (GE.toLabel x) /= (EL.mk "cond") &&
                                (GE.toLabel x) /= (EL.mk "next") && (GE.toLabel x) /= (EL.mk "func")) argEdges)
    possibleFuncs <- mapM GMonad.nodesWithName (map GN.toInst funcNodes) >>= return . concat

    Monad.when (length possibleFuncs < 1) (error "Instruction call has no applicable func candidates")

    r <- liftIO (Random.getStdRandom (Random.randomR (0, length possibleFuncs - 1)))
    (sn, se) <- GMonad.subGraph (possibleFuncs !! r)

    sn' <- GMonad.newNodes (map GN.toInst sn)
    let nodeMap = zip (map GN.toNode sn) (map GN.toNode sn')
        translate = Maybe.fromJust . (flip lookup nodeMap)
    se' <- return (map (\x -> GE.mk (translate . GE.toSrc $ x, translate . GE.toDest $ x, GE.toLabel x)) se)

    let calledNode = GN.mk (translate (GN.toNode (possibleFuncs !! r)), GN.toInst (possibleFuncs !! r))
    ae' <- return (map (\x -> GE.mk (GN.toNode calledNode, GE.toDest x, GE.toLabel x)) ae)

    GMonad.insNodes sn'
    GMonad.insEdges (se' ++ ae')

    GMonad.pushIP calledNode



retI :: GraspM ()
retI = do
    curNode <- GMonad.peekIP >>= return . Maybe.fromJust

    retEdges <- GMonad.edgesOut curNode
    re <- return (filter (\x -> (GE.toLabel x) /= (EL.mk "name") && (GE.toLabel x) /= (EL.mk "next") &&
                                (GE.toLabel x) /= (EL.mk "cond")) retEdges)

    GMonad.popIP
    GMonad.updateIP
    curNode' <- GMonad.peekIP

    Monad.when (Maybe.isJust curNode') (do
        re' <- return (map (\x -> GE.mk (GN.toNode . Maybe.fromJust $ curNode', GE.toDest x, GE.toLabel x)) re)
        GMonad.insEdges re' )



addI :: GraspM ()
addI = do
    curNode <- GMonad.peekIP >>= return . Maybe.fromJust

    argNodes <- GMonad.nodesOut (EL.mk "arg") curNode
    outNodes <- GMonad.nodesOut (EL.mk "out") curNode

    Monad.unless (all (Maybe.isJust . IN.toFloat . GN.toInst) argNodes)
        (error "Instruction add should have numeric arg arguments")

    let input = map (Maybe.fromJust . IN.toFloat . GN.toInst) argNodes
        result = sum input
    mapM_ (GMonad.reLabel (IN.mk (show result))) outNodes

    GMonad.updateIP



mulI :: GraspM ()
mulI = do
    curNode <- GMonad.peekIP >>= return . Maybe.fromJust

    argNodes <- GMonad.nodesOut (EL.mk "arg") curNode
    outNodes <- GMonad.nodesOut (EL.mk "out") curNode

    Monad.unless (all (Maybe.isJust . IN.toFloat . GN.toInst) argNodes)
        (error "Instruction mul should have numeric arg arguments")

    let input = map (Maybe.fromJust . IN.toFloat . GN.toInst) argNodes
        result = product input
    mapM_ (GMonad.reLabel (IN.mk (show result))) outNodes

    GMonad.updateIP



subI :: GraspM ()
subI = do
    curNode <- GMonad.peekIP >>= return . Maybe.fromJust

    leftNodes <- GMonad.nodesOut (EL.mk "left") curNode
    rightNodes <- GMonad.nodesOut (EL.mk "right") curNode
    outNodes <- GMonad.nodesOut (EL.mk "out") curNode

    Monad.when (length leftNodes /= 1) (error "Instruction sub should have one left argument")
    Monad.unless (all (Maybe.isJust . IN.toFloat . GN.toInst) leftNodes)
        (error "Instruction sub should have numeric left arguments")
    Monad.unless (all (Maybe.isJust . IN.toFloat . GN.toInst) rightNodes)
        (error "Instruction sub should have numeric right arguments")

    let inputFunc = Maybe.fromJust . IN.toFloat . GN.toInst
        result = (inputFunc (head leftNodes)) - (sum (map inputFunc rightNodes))
    mapM_ (GMonad.reLabel (IN.mk (show result))) outNodes

    GMonad.updateIP



divI :: GraspM ()
divI = do
    curNode <- GMonad.peekIP >>= return . Maybe.fromJust

    leftNodes <- GMonad.nodesOut (EL.mk "left") curNode
    rightNodes <- GMonad.nodesOut (EL.mk "right") curNode
    outNodes <- GMonad.nodesOut (EL.mk "out") curNode

    Monad.when (length leftNodes /= 1) (error "Instruction div should have one left argument")
    Monad.unless (all (Maybe.isJust . IN.toFloat . GN.toInst) leftNodes)
        (error "Instruction div should have numeric left arguments")
    Monad.unless (all (Maybe.isJust . IN.toFloat . GN.toInst) rightNodes)
        (error "Instruction div should have numeric right arguments")

    let inputFunc = Maybe.fromJust . IN.toFloat . GN.toInst
        result = (inputFunc (head leftNodes)) / (sum (map inputFunc rightNodes))
    mapM_ (GMonad.reLabel (IN.mk (show result))) outNodes

    GMonad.updateIP



modI :: GraspM ()
modI = do
    curNode <- GMonad.peekIP >>= return . Maybe.fromJust

    leftNodes <- GMonad.nodesOut (EL.mk "left") curNode
    rightNodes <- GMonad.nodesOut (EL.mk "right") curNode
    outNodes <- GMonad.nodesOut (EL.mk "out") curNode

    Monad.when (length leftNodes /= 1) (error "Instruction mod should have one left argument")
    Monad.when (length rightNodes /= 1) (error "Instruction mod should have one right argument")
    Monad.unless (all (Maybe.isJust . IN.toInt . GN.toInst) leftNodes)
        (error "Instruction mod should have integer left arguments")
    Monad.unless (all (Maybe.isJust . IN.toInt . GN.toInst) rightNodes)
        (error "Instruction mod should have integer right arguments")

    let inputFunc = Maybe.fromJust . IN.toInt . GN.toInst
        result = (inputFunc (head leftNodes)) `mod` (inputFunc (head rightNodes))
    mapM_ (GMonad.reLabel (IN.mk (show result))) outNodes

    GMonad.updateIP



getcI :: GraspM ()
getcI = do
    curNode <- GMonad.peekIP >>= return . Maybe.fromJust

    outNodes <- GMonad.nodesOut (EL.mk "out") curNode
    fhNodes <- GMonad.nodesOut (EL.mk "fh") curNode

    Monad.when (length fhNodes > 1) (error "Instruction getc should have at most one fh argument")

    let fh = IN.toString . GN.toInst $ head fhNodes
    handle <- if (length fhNodes == 0)
                then liftIO (IO.hSetBuffering IO.stdin IO.NoBuffering) >> return IO.stdin
                else GMonad.getReadHandle fh

    c <- liftIO (IO.hGetChar handle)
    let result = if (c == '\EOT') then -1 else Char.ord c
    mapM_ (GMonad.reLabel (IN.mk (show result))) outNodes

    GMonad.updateIP



putcI :: GraspM ()
putcI = do
    curNode <- GMonad.peekIP >>= return . Maybe.fromJust

    inNodes <- GMonad.nodesOut (EL.mk "in") curNode
    fhNodes <- GMonad.nodesOut (EL.mk "fh") curNode

    Monad.when (length inNodes < 1) (error "Instruction putc should have at least one in argument")
    Monad.when (length fhNodes > 1) (error "Instruction putc should have at most one fh argument")

    r <- liftIO (Random.getStdRandom (Random.randomR (0, length inNodes - 1)))
    let rIn = IN.toInt . GN.toInst $ inNodes !! r

    Monad.unless (Maybe.isJust rIn) (error "Instruction putc should have integer in argument")
    Monad.unless ((>=0) (Maybe.fromJust rIn)) (error "Instruction putc should have positive in argument")

    let fh = IN.toString . GN.toInst $ head fhNodes
    handle <- if (length fhNodes == 0) then return IO.stdout else GMonad.getWriteHandle fh
    liftIO (IO.hPutChar handle (Char.chr (Maybe.fromJust rIn)))

    GMonad.updateIP



getsI :: GraspM ()
getsI = do
    curNode <- GMonad.peekIP >>= return . Maybe.fromJust

    outNodes <- GMonad.nodesOut (EL.mk "out") curNode
    labelNodes <- GMonad.nodesOut (EL.mk "label") curNode
    fhNodes <- GMonad.nodesOut (EL.mk "fh") curNode

    Monad.when (length outNodes /= 1) (error "Instruction gets should have one out argument")
    Monad.when (length labelNodes /= 1) (error "Instruction gets should have one label argument")
    Monad.when (length fhNodes > 1) (error "Instruction gets should have at most one fh argument")

    let fh = IN.toString . GN.toInst $ head fhNodes
    handle <- if (length fhNodes == 0) then return IO.stdin else GMonad.getReadHandle fh
    input <- liftIO (IO.hGetLine handle) >>= (\x -> if (last x == '\n') then return (init x) else return x)

    let unicode = (map Char.ord input) ++ [0]
    nodes <- GMonad.newNodes (map (IN.mk . show) unicode)

    let nextEdges = map (\(x,y) -> GE.mk (GN.toNode x, GN.toNode y, EL.mk "next")) (zip nodes (tail nodes))
        headLabel = EL.mk . IN.toString . GN.toInst $ (head labelNodes)
        headEdge = GE.mk (GN.toNode (head outNodes), GN.toNode (head nodes), headLabel)
    edges <- return (headEdge : nextEdges)

    GMonad.insNodes nodes
    GMonad.insEdges edges

    GMonad.updateIP



putsI :: GraspM ()
putsI = do
    curNode <- GMonad.peekIP >>= return . Maybe.fromJust

    inNodes <- GMonad.nodesOut (EL.mk "in") curNode
    nlNodes <- GMonad.nodesOut (EL.mk "nl") curNode
    fhNodes <- GMonad.nodesOut (EL.mk "fh") curNode

    Monad.when (length inNodes == 0) (error "Instruction puts should have at least one in argument")
    Monad.when (length nlNodes > 1) (error "Instruction puts should have at most one nl argument")
    Monad.when (length fhNodes > 1) (error "Instruction puts should have at most one fh argument")

    let fh = IN.toString . GN.toInst $ head fhNodes
    handle <- if (length fhNodes == 0) then return IO.stdout else GMonad.getWriteHandle fh

    let nl = IN.toFloat . GN.toInst $ head nlNodes
    newLine <- if (length nlNodes == 0 || Maybe.isNothing nl || Maybe.fromJust nl /= 0)
                then return "\n" else return ""

    r <- liftIO (Random.getStdRandom (Random.randomR (0, length inNodes - 1)))
    let input = inNodes !! r

    let decodeFunc n acc = do
            let curValue = IN.toInt . GN.toInst $ n
            nextNodes <- GMonad.nodesOut (EL.mk "next") n
            Monad.when (length nextNodes > 1) (error "Stack nodes should have at most one next edge")
            Monad.when (Maybe.isNothing curValue) (error "Instruction puts requires a stack with integer values")
            if (length nextNodes == 0)
                then if (Maybe.fromJust curValue /= 0)
                        then error "Instruction puts requires a zero terminated stack"
                        else return (reverse acc)
                else decodeFunc (head nextNodes) ((Char.chr . Maybe.fromJust $ curValue):acc)

    output <- if (Maybe.isNothing . IN.toInt . GN.toInst $ input)
                then return (IN.toString . GN.toInst $ input)
                else decodeFunc input []
    liftIO (IO.hPutStr handle (output ++ newLine))

    GMonad.updateIP

