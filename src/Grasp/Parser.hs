module Grasp.Parser (
    GraspProgram(..),

    parseGrasp,
    ) where


import Control.Applicative( some )
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Text.Parsec.Pos
import Data.Graph.Inductive.Graph( Node, LNode, LEdge, (&) )
import Data.Graph.Inductive.Graph as Graph
import Data.Graph.Inductive.Tree




type GraspProgram = Gr String String




parseGrasp :: String -> Either ParseError GraspProgram
parseGrasp input = parse grasp "error" input >>= validate



validate :: ([LNode String],[LEdge String]) -> Either ParseError GraspProgram
validate (nodeList, edgeList) =
	let nameEdges = filter (\(_,_,z) -> z == "name") edgeList

	    nameNodes = map (\(_,x,_) -> x) nameEdges
	    nameLNodes = filter (\(x,_) -> x `elem` nameNodes) nodeList

	    a = multiNames nameEdges
	    b = nonStringNames nameLNodes
	    c = noGraspMain nameLNodes

	in case (a,b,c) of
		([],[],False) -> Right (Graph.mkGraph nodeList edgeList)

		([],[],True) -> Left (newErrorMessage (Message "could not find grasp:main") (newPos "" 0 0))

		([],(x:_),_) -> Left (newErrorMessage (Message ("node " ++ (show x) ++ " has a numeric name")) (newPos "" 0 0))

		((x:_),_,_) -> Left (newErrorMessage (Message ("node " ++ (show x) ++ " has multiple names")) (newPos "" 0 0))



multiNames :: [LEdge String] -> [LNode String]
multiNames x = []



nonStringNames :: [LNode String] -> [LNode String]
nonStringNames x = []



noGraspMain :: [LNode String] -> Bool
noGraspMain x = False




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

