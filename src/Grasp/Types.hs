module Grasp.Types (
	Instruction(..),
	EdgeLabel(..),
	GNode(..),
	GEdge(..),

	gnode,
	gninst,
	gefrom,
	geto,
	gelabel
    ) where




import Grasp.Graph( Node, LNode, LEdge )




newtype Instruction = Instruction String
    deriving (Eq, Show)

newtype EdgeLabel = EdgeLabel String
    deriving (Eq, Show)

newtype GNode = GNode (LNode Instruction)
    deriving (Eq, Show)

newtype GEdge = GEdge (LEdge EdgeLabel)
    deriving (Eq, Show)




gnode :: GNode -> Node
gnode (GNode a) = fst a

gninst :: GNode -> Instruction
gninst (GNode a) = snd a




gefrom :: GEdge -> Node
gefrom (GEdge (a,_,_)) = a

geto :: GEdge -> Node
geto (GEdge (_,b,_)) = b

gelabel :: GEdge -> EdgeLabel
gelabel (GEdge (_,_,c)) = c

