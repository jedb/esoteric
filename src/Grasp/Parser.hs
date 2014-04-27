module Grasp.Parser (
    GraspProgram(..),
    GraspNode,

    parseGrasp,

    getNodeLabel,
    getFloat,
    getString
    ) where


import Control.Applicative( some )
import Text.ParserCombinators.Parsec
import Data.Graph.Inductive.Graph( Node, LNode, LEdge, (&) )
import Data.Graph.Inductive.Graph as Graph
import Data.Graph.Inductive.Tree




data GraspProgram = Gr GraspNode GraspLabel


data GraspNode  = GFloat GraspLabel Float
                | GString GraspLabel String
                | GEmpty GraspLabel


type GraspLabel = String




parseGrasp :: String -> Either ParseError ([LNode String],[LEdge String])
parseGrasp = parse grasp "error"
--parseGrasp input =
--	let firstPass = parse grasp "error" input
--	in case firstPass of
--		Left e -> Left e
--		Right (n,e) -> validate n e



--validate :: [LNode String] -> [LEdge String] -> Either ParseError GraspProgram



getNodeLabel :: GraspNode -> GraspLabel
getNodeLabel (GFloat label _) = label
getNodeLabel (GString label _) = label
getNodelabel (GEmpty label) = label



getFloat :: GraspNode -> Maybe Float
getFloat (GFloat _ f) = Just f
getFloat _ = Nothing



getString :: GraspNode -> Maybe String
getString (GString _ s) = Just s
getString _ = Nothing




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

