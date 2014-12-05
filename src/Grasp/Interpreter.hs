module Grasp.Interpreter (
    grasp
    ) where




import qualified Control.Monad as Monad

import qualified Data.Maybe as Maybe

import Grasp.Monad( GraspM )
import qualified Grasp.Monad as GMonad

import Grasp.Types.GNode( GNode )
import qualified Grasp.Types.GNode as GN

import Grasp.Types.GEdge( GEdge )
import qualified Grasp.Types.GEdge as GE

import Grasp.Types.Instruction( Instruction )
import qualified Grasp.Types.Instruction as IN




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
setI = GMonad.updateIP



newI :: GraspM ()
newI = GMonad.updateIP



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

