module Grasp.Types.GEdge (
	GEdge,

	mk,

	toSrc,
	toDest,
	toLabel,
	toLEdge
	) where




import Grasp.Graph( Node, LEdge )
import Grasp.Types.EdgeLabel( EdgeLabel )




newtype GEdge = GEdge (LEdge EdgeLabel)
    deriving (Show, Eq)




mk :: LEdge EdgeLabel -> GEdge
mk = GEdge

toSrc :: GEdge -> Node
toSrc (GEdge (x,_,_)) = x

toDest :: GEdge -> Node
toDest (GEdge (_,y,_)) = y

toLabel :: GEdge -> EdgeLabel
toLabel (GEdge (_,_,z)) = z

toLEdge :: GEdge -> LEdge EdgeLabel
toLEdge (GEdge e) = e

