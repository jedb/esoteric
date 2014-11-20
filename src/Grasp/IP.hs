module Grasp.IP (
	IP,

    singleton,
    peek,
    push,
    pop,
    isEmpty
	) where



import Grasp.Node( GNode )
import qualified Grasp.Node as GN



type IP = [GNode]



singleton :: GNode -> IP
singleton = (:[])

peek :: IP -> GNode
peek = head

push :: GNode -> IP -> IP
push = (:)

pop :: IP -> IP
pop = tail

isEmpty :: IP -> Bool
isEmpty = (==[])

