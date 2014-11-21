module Grasp.Graph (
	Node,
	LNode,
	UNode,
	
	Edge,
	LEdge,
	UEdge,
	
	Adj,
	
	Context,
	MContext,
	UContext,
	
	Decomp,
	GDecomp,
	UDecomp,

	Path,
	LPath,
	UPath,

    Gr,

	empty,
	isEmpty,
	match,
	mkGraph,
	labNodes,
	noNodes,
	labEdges,
	nodes
    ) where




import Data.List
import qualified Data.Maybe as Maybe




type Node = String

type LNode a = (Node, a)

type UNode = LNode ()

type Edge = (Node, Node)

type LEdge a = (Node, Node, a)

type UEdge = LEdge ()

type Adj b = [(b, Node)]

type Context a b = (Adj b, Node, a, Adj b)

type MContext a b = Maybe (Context a b)

type UContext = ([Node], Node, [Node])

type Decomp a b = (MContext a b, Gr a b)

type GDecomp a b = (Context a b, Gr a b)

type UDecomp = (Maybe UContext, Gr () ())

type Path = [Node]

newtype LPath a = LP [LNode a]

type UPath = [UNode]




data Gr a b = Gr { labNodes :: [LNode a]
                 , labEdges :: [LEdge b] }
    deriving (Show)




instance (Eq a, Eq b) => Eq (Gr a b) where
	a == b   =   (labNodes a == labNodes b) && (labEdges a == labEdges b)




empty :: Gr a b
empty = Gr [] []



isEmpty :: Gr a b -> Bool
isEmpty gr = (length (labNodes gr) == 0) && (length (labEdges gr) == 0)



match :: Node -> Gr a b -> Decomp a b
match n gr =
    if (n `notElem` nodes gr)
    	then (Nothing, gr)
    	else (Just (to, n, label, from), gr)
    where
    	to = map edgeToAdjTo (filter (edgeTo n) (labEdges gr))
    	label = snd . head $ (filter (\(x,y) -> x == n) (labNodes gr))
    	from = map edgeToAdjFrom (filter (edgeFrom n) (labEdges gr))



mkGraph :: [LNode a] -> [LEdge b] -> Gr a b
mkGraph lnodes ledges =
	let nodes = map fst lnodes
	    edgeNodes = (map (\(x,y,z) -> x) ledges) `union` (map (\(x,y,z) -> y) ledges)

	in if (all (`elem` nodes) edgeNodes)
		then Gr lnodes ledges
		else error "Edge Exception"



noNodes :: Gr a b -> Int
noNodes = length . labNodes



nodes :: Gr a b -> [Node]
nodes gr = (map fst) . labNodes $ gr



edgeToAdjFrom :: LEdge a -> Adj a
edgeToAdjFrom (x,y,z) = (z,y)



edgeToAdjTo :: LEdge a -> Adj a
edgeToAdjTo (x,y,z) = (z,x)



edgeFrom :: Node -> LEdge a -> Bool
edgeFrom n (x,y,z) = (y == n)



edgeTo :: Node -> LEdge a -> Bool
edgeTo n (x,y,z) = (x == n)

