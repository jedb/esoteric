module Grasp.GNode (
	GNode,
	Instruction,

    mkGNode,
    mkInst,

	toNode,
	toInst,
	toLNode,

	instToFloat
	) where




import Grasp.Graph( Node, LNode )
import Text.Read( readMaybe )




newtype Instruction = Instruction String
    deriving (Show, Eq)

newtype GNode = GNode (LNode Instruction)
    deriving (Show, Eq)




mkGNode :: LNode Instruction -> GNode
mkGNode = GNode



mkInst :: String -> Instruction
mkInst = Instruction



toNode :: GNode -> Node
toNode (GNode n) = fst n



toInst :: GNode -> Instruction
toInst (GNode n) = snd n



toLNode :: GNode -> LNode Instruction
toLNode (GNode n) = n



instToFloat :: Instruction -> Maybe Float
instToFloat (Instruction i) = readMaybe i

