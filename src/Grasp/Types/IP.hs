module Grasp.Types.IP (
	IP,

    singleton,
    empty,
    isEmpty,
    peek,
    push,
    pop,
    shift,

    toList
	) where




import Grasp.Types.GNode( GNode )




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
pop (IP p) = if (length p == 0) then empty else IP (tail p)

shift :: GNode -> IP -> IP
shift n (IP p) = if (length p == 0) then empty else IP (n:(tail p))

toList :: IP -> [GNode]
toList (IP p) = p

