module Grasp.Interpreter (
    grasp
    ) where




import qualified System.Random as Random

import qualified Control.Monad as Monad

import Control.Monad.IO.Class( liftIO )

import qualified Data.Maybe as Maybe

import Data.List( (!!) )
import qualified Data.List as List

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
delI = GMonad.updateIP



pushI :: GraspM ()
pushI = GMonad.updateIP



implicitPushI :: GraspM ()
implicitPushI = GMonad.updateIP



popI :: GraspM ()
popI = GMonad.updateIP



pickI :: GraspM ()
pickI = GMonad.updateIP



callI :: GraspM ()
callI = GMonad.updateIP



retI :: GraspM ()
retI = GMonad.updateIP



addI :: GraspM ()
addI = GMonad.updateIP



mulI :: GraspM ()
mulI = GMonad.updateIP



subI :: GraspM ()
subI = GMonad.updateIP



divI :: GraspM ()
divI = GMonad.updateIP



modI :: GraspM ()
modI = GMonad.updateIP



getcI :: GraspM ()
getcI = GMonad.updateIP



putcI :: GraspM ()
putcI = GMonad.updateIP



getsI :: GraspM ()
getsI = GMonad.updateIP



putsI :: GraspM ()
putsI = GMonad.updateIP

