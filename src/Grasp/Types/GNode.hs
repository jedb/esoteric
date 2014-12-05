module Grasp.Types.GNode (
	GNode,

    mk,

	toNode,
	toInst,
	toLNode
	) where




import Grasp.Graph( Node, LNode )
import Grasp.Types.Instruction( Instruction )




newtype GNode = GNode (LNode Instruction)
    deriving (Show, Eq)




mk :: LNode Instruction -> GNode
mk = GNode

toNode :: GNode -> Node
toNode (GNode n) = fst n

toInst :: GNode -> Instruction
toInst (GNode n) = snd n

toLNode :: GNode -> LNode Instruction
toLNode (GNode n) = n

