module Grasp.Graph (
	Node, LNode, UNode,
	Edge, LEdge, UEdge,
	Adj,
	Context, MContext, UContext,
	Decomp, GDecomp, UDecomp,
	Path, LPath, UPath,

    Gr,

	empty,
	isEmpty,
	match,
	mkGraph,
	labNodes,
	matchAny,
	noNodes,
	nodeRange,
	labEdges,

	(&),

	nodes,
	edges,
	newNodes,
	gelem,

	insNode, insEdge,
	delNode, delEdge, delLEdge,
	insNodes, insEdges,
	delNodes, delEdges,
	buildGr,
	mkUGraph,

	context,
	lab,
	neighbours,
	suc, pre, lsuc, lpre,
	out, inn,
	outdeg, indeg, deg,
	equal
    ) where




import qualified Data.List as List
import qualified Data.Maybe as Maybe




-- this whole thing is essentially a reimplementation of Data.Graph.Inductive.Graph
-- with String nodes instead of Int nodes, because it makes the rest of the code easier



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




data Gr a b = Gr { getLabNodes :: [LNode a]
                 , getLabEdges :: [LEdge b] }
    deriving (Show)




instance (Eq a, Eq b) => Eq (Gr a b) where
	a == b   =   (labNodes a == labNodes b) && (labEdges a == labEdges b)





-- class methods


empty :: Gr a b
empty = Gr [] []



isEmpty :: Gr a b -> Bool
isEmpty gr = (length (labNodes gr) == 0)



match :: Node -> Gr a b -> Decomp a b
match n gr =
    if (n `notElem` nodes gr)
    	then (Nothing, gr)
    	else (Just (to, n, label, from), (delNode n gr))
    where
    	to = map (\(x,y,z) -> (z,x)) (inn gr n)
    	label = snd . head $ (filter (\(x,y) -> x == n) (labNodes gr))
    	from = map (\(x,y,z) -> (z,y)) (out gr n)



mkGraph :: [LNode a] -> [LEdge b] -> Gr a b
mkGraph lnodes ledges =
	let nodes = map fst lnodes
	    edgeNodes = (map (\(x,y,z) -> x) ledges) `List.union` (map (\(x,y,z) -> y) ledges)
	in if (all (`elem` nodes) edgeNodes)
		then Gr lnodes ledges
		else error "Edge Exception"



labNodes :: Gr a b -> [LNode a]
labNodes = getLabNodes



matchAny :: Gr a b -> GDecomp a b
matchAny gr =
	let (mcon, gr') = match (head . nodes $ gr) gr
	in if (isEmpty gr)
		then error "Match Exception"
		else (Maybe.fromJust mcon, gr')



noNodes :: Gr a b -> Int
noNodes = length . labNodes



nodeRange :: Gr a b -> (Node,Node)
nodeRange gr =
	let nodes = map fst (labNodes gr)
	in if (length nodes == 0) then ("","") else (head nodes, last nodes)



labEdges :: Gr a b -> [LEdge b]
labEdges = getLabEdges



(&) :: Context a b -> Gr a b -> Gr a b
(to, n, lab, from) & gr =
	let edgesTo = map (\(z,y) -> (y,n,z)) to
	    edgesFrom = map (\(z,x) -> (n,x,z)) from
	in (insEdges edgesTo) . (insEdges edgesFrom) . (insNode (n,lab)) $ gr





-- graph projection


nodes :: Gr a b -> [Node]
nodes gr = map fst (labNodes gr)



edges :: Gr a b -> [Edge]
edges gr = map (\(x,y,z) -> (x,y)) (labEdges gr)



newNodes :: Int -> Gr a b -> [Node]
newNodes x gr = take x (filter (`notElem` (nodes gr)) (map show [1..]))



gelem :: Node -> Gr a b -> Bool
gelem n gr = n `elem` (nodes gr)





-- graph construction and deconstruction


insNode :: LNode a -> Gr a b -> Gr a b
insNode n gr =
	let preExisting = filter (\x -> fst x == fst n) (labNodes gr)
	in if (length preExisting /= 0)
		then error "Node Exception"
		else Gr (n:(labNodes gr)) (labEdges gr)



insEdge :: LEdge b -> Gr a b -> Gr a b
insEdge (a,b,c) gr =
	let from = filter (\x -> fst x == a) (labNodes gr)
	    to = filter (\x -> fst x == b) (labNodes gr)
	in if (length from == 0 || length to == 0)
		then error "Edge Exception"
		else Gr (labNodes gr) ((a,b,c):(labEdges gr))



delNode :: Node -> Gr a b -> Gr a b
delNode n gr =
	let nodes' = filter (\x -> fst x /= n) (labNodes gr)
	    edges' = filter (\(x,y,z) -> x /= n && y /= n) (labEdges gr)
	in Gr nodes' edges'



delEdge :: Edge -> Gr a b -> Gr a b
delEdge e gr =
	let edges' = filter (\(x,y,z) -> (x,y) /= e) (labEdges gr)
	in Gr (labNodes gr) edges'



delLEdge :: (Eq b) => LEdge b -> Gr a b -> Gr a b
delLEdge e gr = Gr (labNodes gr) (filter (/= e) (labEdges gr))



insNodes :: [LNode a] -> Gr a b -> Gr a b
insNodes ns gr = List.foldl' (flip insNode) gr ns



insEdges :: [LEdge b] -> Gr a b -> Gr a b
insEdges es gr = List.foldl' (flip insEdge) gr es



delNodes :: [Node] -> Gr a b -> Gr a b
delNodes ns gr =
	let nodes' = filter (\x -> fst x `notElem` ns) (labNodes gr)
	    edges' = filter (\(x,y,z) -> x `notElem` ns && y `notElem` ns) (labEdges gr)
	in Gr nodes' edges'



delEdges :: [Edge] -> Gr a b -> Gr a b
delEdges es gr =
	let edges' = filter (\(x,y,z) -> (x,y) `notElem` es) (labEdges gr)
	in Gr (labNodes gr) edges'



buildGr :: [Context a b] -> Gr a b
buildGr cs = List.foldl' (flip (&)) empty cs



mkUGraph :: [Node] -> [Edge] -> Gr () ()
mkUGraph ns es = Gr (map (\x -> (x,())) ns) (map (\(x,y) -> (x,y,())) es)





-- graph inspection


context :: Gr a b -> Node -> Context a b
context gr n =
	let from = map (\(x,y,z) -> (z,y)) (out gr n)
	    to = map (\(x,y,z) -> (z,x)) (inn gr n)
	in if (n `notElem` (nodes gr))
		then error "Match Exception"
		else (to, n, Maybe.fromJust (lab gr n), from)



lab :: Gr a b -> Node -> Maybe a
lab gr n =
	let nlist = filter (\(x,y) -> x == n) (labNodes gr)
	in if (length nlist == 0) then Nothing else Just (snd . head $ nlist)



neighbours :: Gr a b -> Node -> [Node]
neighbours gr n = (suc gr n) ++ (pre gr n)



suc :: Gr a b -> Node -> [Node]
suc gr n =
	if (n `notElem` (nodes gr))
		then error "Match Exception"
		else map (\(x,y,z) -> y) (out gr n)



pre :: Gr a b -> Node -> [Node]
pre gr n =
	if (n `notElem` (nodes gr))
		then error "Match Exception"
		else map (\(x,y,z) -> x) (inn gr n)



lsuc :: Gr a b -> Node -> [(Node, b)]
lsuc gr n = map (\(x,y,z) -> (y,z)) (out gr n)



lpre :: Gr a b -> Node -> [(Node, b)]
lpre gr n = map (\(x,y,z) -> (x,z)) (inn gr n)



out :: Gr a b -> Node -> [LEdge b]
out gr n = filter (\(x,y,z) -> x == n) (labEdges gr)



inn :: Gr a b -> Node -> [LEdge b]
inn gr n = filter (\(x,y,z) -> y == n) (labEdges gr)



outdeg :: Gr a b -> Node -> Int
outdeg gr n = length (out gr n)



indeg :: Gr a b -> Node -> Int
indeg gr n = length (inn gr n)



deg :: Gr a b -> Node -> Int
deg gr n = (outdeg gr n) + (indeg gr n)



equal :: (Eq a, Eq b) => Gr a b -> Gr a b -> Bool
equal a b = (a == b)

