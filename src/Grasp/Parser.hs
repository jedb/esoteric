module Grasp.Parser (
    GraspProgram(..),
    GraspData(..),

    parseGrasp,
    nodesWithName,
    iso,
    dup
    ) where


import Control.Applicative( some )
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Text.Parsec.Pos
import Text.Read( readMaybe )
import Data.Graph.Inductive.Graph( Node, LNode, LEdge, (&) )
import Data.Graph.Inductive.Graph as Graph
import Data.Graph.Inductive.Tree
import Data.List
import Data.Maybe




type GraspProgram = Gr String String

type StrLNode a = (String,a)

type StrLEdge a = (String,String,a)

type GraspData = ([StrLNode String],[StrLEdge String])



instance (Ord a, Ord b) => Eq (Gr a b) where
	a == b  =   ((sort . Graph.labNodes $ a) == (sort . Graph.labNodes $ b)) &&
	            ((sort . Graph.labEdges $ a) == (sort . Graph.labEdges $ b))




parseGrasp :: String -> Either ParseError GraspProgram
parseGrasp input =
	parse grasp "error" input >>= sanityCheck >>= nameCheck >>= return . constructGraph



sanityCheck :: ([LNode String],[LEdge String]) -> Either ParseError ([LNode String],[LEdge String])
sanityCheck (nodeList, edgeList) = Right (nodeList, edgeList)



nameCheck :: ([LNode String],[LEdge String]) -> Either ParseError ([LNode String],[LEdge String])
nameCheck (nodeList, edgeList) =
	let nameEdges = filter (\(_,_,z) -> z == "name") edgeList

        -- designed to convert the edges into (lnode,name) pairs
	    findNode n l = find (\(x,_) -> x == n) l
	    mapFunc (x,y,_) = (fromJust (findNode x nodeList), snd . fromJust $ (findNode y nodeList))

	    namedNodes = map mapFunc nameEdges

	    a = multiNames namedNodes
	    b = nonStringNames namedNodes
	    c = graspMainPresent namedNodes

	in case (a,b,c) of
		(Just x,_,_) -> Left (newErrorMessage (Message ("node " ++ (show x) ++ " has multiple names")) (newPos "" 0 0))

		(_,Just x,_) -> Left (newErrorMessage (Message ("node " ++ (show x) ++ " has a numeric name")) (newPos "" 0 0))

		(_,_,False) -> Left (newErrorMessage (Message "could not find grasp:main") (newPos "" 0 0))

		(_,_,_) -> Right (nodeList, edgeList)



multiNames :: [(LNode String, String)] -> Maybe (LNode String)
multiNames = dup . (map fst)



nonStringNames :: [(LNode String, String)] -> Maybe (LNode String)
nonStringNames nodeList =
	let f x = readMaybe x :: Maybe Float
	    nonStringNames = filter (isJust . f . snd) nodeList
	in if (nonStringNames == []) then Nothing else Just (fst . head $ nonStringNames)



graspMainPresent :: [(LNode String, String)] -> Bool
graspMainPresent = any (\x -> snd x == "grasp:main")



constructGraph :: ([LNode String],[LEdge String]) -> GraspProgram
constructGraph = uncurry Graph.mkGraph



nodesWithName :: GraspProgram -> String -> [LNode String]
nodesWithName g s = []



normalise :: GraspData -> ([LNode String],[LEdge String])
normalise (n,e) = ([],[])



iso :: GraspProgram -> GraspProgram -> Bool
iso a b =
	let f (x,y) = (show x, y)
	    g (x,y,z) = (show x, show y, z)

        -- converts a grasp program into grasp data
	    h x = ((map f) . Graph.labNodes $ x, (map g) . Graph.labEdges $ x)

	in (constructGraph . normalise . h $ a) == (constructGraph . normalise . h $ b)



dup :: (Eq a) => [a] -> Maybe a
dup x =
	let dup' [] _ = Nothing
	    dup' (x:xs) s = if (x `elem` s) then Just x else dup' xs (x:s)
	in dup' x []




grasp = do
	string "digraph {"
	whiteSpace
	(n,e) <- stmtList ([],[])
	string "}"
	eol
	eof
	return (n,e)


stmtList (n,e) =
	    try (node >>= (\x -> stmtList (x:n,e)) )
	<|> try (edge >>= (\x -> stmtList (n,x:e)) )
	<|> return (reverse n, reverse e)


node = do
	i <- ident
	l <- labelAttrib
	whiteSpace
	return (i,l)


edge = do
	a <- ident
	directedEdge
	b <- ident
	l <- labelAttrib
	whiteSpace
	return (a,b,l)


ident = do
	d <- some digit
	inLineWhSp
	return (read d)


labelAttrib = do
	char '['
	inLineWhSp
	string "label=\""
	l <- labelID
	char '\"'
	inLineWhSp
	string "];"
	return l


labelID = some (noneOf "\"\r\n\\" <|> escapedChar)


escapedChar  =  try (string "\\\"" >> return '\"')
            <|> try (string "\\\\" >> return '\\')


directedEdge = string "->" >> inLineWhSp


inLineWhSp = many (oneOf "\t ")
whiteSpace = many (oneOf "\n\r\t ")


eol  =  try (string "\r\n")
    <|> try (string "\n\r")
    <|> try (string "\n")
    <|> try (string "\r")
    <?> "end of line"

