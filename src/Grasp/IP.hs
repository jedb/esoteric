module Grasp.IP (
	IP,

    singleton,
    peek,
    push,
    pop,
    isEmpty
	) where




import Grasp.Types( GNode )




newtype IP = IP [GNode]
    deriving (Eq, Show)




singleton :: GNode -> IP
singleton n = IP [n]

peek :: IP -> GNode
peek (IP p) = head p

push :: GNode -> IP -> IP
push n (IP p) = IP (n:p)

pop :: IP -> IP
pop (IP p) = IP (tail p)

isEmpty :: IP -> Bool
isEmpty (IP p) = (length p == 0)

