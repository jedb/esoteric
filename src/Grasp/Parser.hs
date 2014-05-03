module Grasp.Parser (
    parseGrasp,
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
import qualified Data.Map as Map
import Grasp.Types




type StrLNode a = (String,a)

type StrLEdge a = (String,String,a)

type GraspData = ([StrLNode String],[StrLEdge String])




parseGrasp :: String -> Either ParseError GraspProgram
parseGrasp input =
	parse grasp "error" input >>= sanityCheck >>= nameCheck >>= return . constructGraph



sanityCheck :: GraspData -> Either ParseError GraspData
sanityCheck (nodeList, edgeList) =
	let n = map fst nodeList

	    a = multiCheck nodeList
	    b = filter (\(x,y,_) -> x `notElem` n || y `notElem` n) edgeList

	in case (a,b) of
		(Just x,_) -> Left (newErrorMessage (Message ("multiple declaration of node " ++ (show x))) (newPos "" 0 0))

		(_,(x:_)) -> Left (newErrorMessage (Message ("edge " ++ (show x) ++ " is unconnected")) (newPos "" 0 0))

		_ -> Right (nodeList, edgeList)



nameCheck :: GraspData -> Either ParseError GraspData
nameCheck (nodeList, edgeList) =
	let nameEdges = filter (\(_,_,z) -> z == "name") edgeList

        -- designed to convert the edges into (lnode,name) pairs
	    findNode n l = find (\(x,_) -> x == n) l
	    mapFunc (x,y,_) = (fromJust (findNode x nodeList), snd . fromJust $ (findNode y nodeList))

	    namedNodes = map mapFunc nameEdges

	    a = multiCheck namedNodes
	    b = nonStringNames namedNodes
	    c = graspMainPresent namedNodes

	in case (a,b,c) of
		(Just x,_,_) -> Left (newErrorMessage (Message ("node " ++ (show x) ++ " has multiple names")) (newPos "" 0 0))

		(_,Just x,_) -> Left (newErrorMessage (Message ("node " ++ (show x) ++ " has a numeric name")) (newPos "" 0 0))

		(_,_,False) -> Left (newErrorMessage (Message "could not find grasp:main") (newPos "" 0 0))

		_ -> Right (nodeList, edgeList)



multiCheck :: (Eq a) => [(a, b)] -> Maybe a
multiCheck = dup . (map fst)



nonStringNames :: (Eq a) => [(a, String)] -> Maybe a
nonStringNames nodeList =
	let f x = readMaybe x :: Maybe Float
	    nonStringNames = filter (isJust . f . snd) nodeList
	in if (nonStringNames == []) then Nothing else Just (fst . head $ nonStringNames)



graspMainPresent :: [(a, String)] -> Bool
graspMainPresent = any (\x -> snd x == "grasp:main")



constructGraph :: GraspData -> GraspProgram
constructGraph (sn, se) =
	let strNodeList = map fst sn
	    nmap = Map.fromList (zip strNodeList [1..])
	    change x = fromJust (Map.lookup x nmap)
	    n = map (\(x,y) -> (change x, y)) sn
	    e = map (\(x,y,z) -> (change x, change y, z)) se
	in Graph.mkGraph n e



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
	d <- some (noneOf " \t\r\n")
	inLineWhSp
	return d


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

