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




import Grasp.Graph( Node )




newtype IP = IP [Node]
    deriving (Eq, Show)




singleton :: Node -> IP
singleton n = IP [n]

empty :: IP
empty = IP []

isEmpty :: IP -> Bool
isEmpty (IP p) = (length p == 0)

peek :: IP -> Maybe Node
peek (IP p) = if (length p == 0) then Nothing else Just (head p)

push :: Node -> IP -> IP
push n (IP p) = IP (n:p)

pop :: IP -> IP
pop (IP p) = if (length p == 0) then empty else IP (tail p)

shift :: Node -> IP -> IP
shift n (IP p) = if (length p == 0) then empty else IP (n:(tail p))

toList :: IP -> [Node]
toList (IP p) = p

