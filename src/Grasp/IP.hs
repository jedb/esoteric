module Grasp.IP (
	IP,

    singleton,
    empty,
    isEmpty,
    peek,
    push,
    pop
	) where




import Grasp.Types( GNode )




newtype IP = IP [GNode]
    deriving (Eq, Show)




singleton :: GNode -> IP
singleton n = IP [n]

empty :: IP
empty = IP []

isEmpty :: IP -> Bool
isEmpty (IP p) = (length p == 0)

peek :: IP -> Maybe GNode
peek (IP p) = if (length p == 0) then Nothing else Just (head p)

push :: GNode -> IP -> IP
push n (IP p) = IP (n:p)

pop :: IP -> IP
pop (IP p) = if (length p == 0) then IP p else IP (tail p)

