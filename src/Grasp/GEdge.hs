module Grasp.GEdge (
	GEdge,
	EdgeLabel,

	mkGEdge,
	mkLabel,

	toSrc,
	toDest,
	toLabel,
	toLEdge
	) where




import Grasp.Graph( Node, LEdge )




newtype EdgeLabel = EdgeLabel String
    deriving (Show, Eq)

newtype GEdge = GEdge (LEdge EdgeLabel)
    deriving (Show, Eq)




mkGEdge :: LEdge EdgeLabel -> GEdge
mkGEdge = GEdge



mkLabel :: String -> EdgeLabel
mkLabel = EdgeLabel



toSrc :: GEdge -> String
toSrc (GEdge (x,_,_)) = x



toDest :: GEdge -> String
toDest (GEdge (_,y,_)) = y



toLabel :: GEdge -> EdgeLabel
toLabel (GEdge (_,_,z)) = z



toLEdge :: GEdge -> LEdge EdgeLabel
toLEdge (GEdge e) = e

