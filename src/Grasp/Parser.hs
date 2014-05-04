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
import Data.Char
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
	whiteSpac'
	(n,e) <- stmtLis' ([],[])
	string "}"
	eo'
	eof
	return (n,e)


stmtLis' (n,e) =
	    try (nod' >>= (\x -> stmtLis' (x:n,e)) )
	<|> try (edg' >>= (\x -> stmtLis' (n,x:e)) )
	<|> return (reverse n, reverse e)


nod' = do
	i <- iden'
	l <- labelAttri'
	whiteSpac'
	return (i,l)


edg' = do
	a <- iden'
	directedEdg'
	b <- iden'
	l <- labelAttri'
	whiteSpac'
	return (a,b,l)


iden' = do
	d <- some (noneOf " \t\r\n")
	inLineWhS'
	return d


labelAttri' = do
	char '['
	inLineWhS'
	string "label=\""
	l <- labelI'
	char '\"'
	inLineWhS'
	string "];"
	return l


labelI' = some (noneOf "\"\r\n\\" <|> escapedCha')


escapedCha'  =  try (string "\\\"" >> return '\"')
            <|> try (string "\\\\" >> return '\\')


directedEdg' = string "->" >> inLineWhS'


inLineWhS' = many (oneOf "\t ")
whiteSpac' = many (oneOf "\n\r\t ")


eo'  =  try (string "\r\n")
    <|> try (string "\n\r")
    <|> try (string "\n")
    <|> try (string "\r")
    <?> "end of line"





removeComments = gline `sepEndBy` eol >>= return . concat


eol  =  try (string "\r\n")
    <|> try (string "\n\r")
    <|> try (string "\n")
    <|> try (string "\r")
    <?> "end of line"


gline = many thing >>= return . concat


thing = try (some (noneOf "\r\n\"/#"))
    <|> try (quotedString >>= return . ("\"" ++) . (++ "\""))
    <|> try singleLineComment
    <|> try multiLineComment
    <|> (anyChar >>= return . (:[]) )


singleLineComment =
	    (string "//" >> many (noneOf "\r\n") >> return "")
	<|> (string "#" >> many (noneOf "\r\n") >> return "")


multiLineComment = do
	string "/*"
	many (noneOf "*" <|> (char '*' >> notFollowedBy (char '/') >> return '*'))
	return ""

-- work in progress more complete DOT language parser below this point

graspDOT = do
	optional strict
	graphType
	optional ident
	openBrace
	(n,e) <- stmtList ([],[])
	closeBrace
	eof
	return (n,e)


strict = caseInsensitiveString "strict" >>= (\x -> whiteSpace >> return x)


graphType = try (caseInsensitiveString "digraph" >>= (\x -> whiteSpace >> return x)) <?> "digraph"


ident = ((try alphaNumString)
    <|>  (try numeral)
    <|>  (try quotedString)) >>= (\x -> whiteSpace >> return x)
    <?> "ID"


stmtList (n,e) =
	    try (node >>= (\x -> stmtList (x:n,e)) )
	<|> try (edge >>= (\x -> stmtList (n,x:e)) )
	<|> try (attr >> stmtList (n,e))
	<|> try (subgraph >>= (\(x,y) -> stmtList ((reverse x) ++ n, (reverse y) ++ e)) )
	<|> try (blankLine >> stmtList (n,e))
	<|> return (reverse n, reverse e)


blankLine = whiteSpace >> eol


alphaNumString = do
	a <- nonDigitChar
	b <- many alphaNumChar
	return (a:b)


nonDigitChar = letter <|> char '_'
alphaNumChar = alphaNum <|> char '_'


numeral = try negativeNum <|> positiveNum
negativeNum = char '-' >> positiveNum >>= return . ('-':)
positiveNum = try pointNum <|> try floatNum <|> wholeNum
pointNum = char '.' >> some digit >>= return . ('.':)
wholeNum = some digit
floatNum = do { a <- some digit; char '.'; b <- many digit; return (a ++ "." ++ b)}


quotedString = do
	char '\"'
	s <- some quotedChar
	char '\"'
	return s


quotedChar = noneOf "\"\r\n" <|> try (char '\\' >> char '\"')


node = do
	n <- ident
	a <- attrList
	return (n,a)


edge = do
	a <- ident
	edgeOp
	b <- ident
	c <- attrList
	return (a,b,c)


edgeOp = string "->" >> whiteSpace >> return "->"


attr = attrType >> attrList
attrType =  caseInsensitiveString "graph"
        <|> caseInsensitiveString "node"
        <|> caseInsensitiveString "edge"


attrList = do
	a <- many aList
	let r = filter (\x -> fst x == "label") (concat a)
	case (length r) of
		0 -> fail "expected node/edge label"
		1 -> return . snd . head $ r
		_ -> fail "unexpected multiple labels for single node/edge"


aList = do
	openBracket
	a <- many equAttr
	closeBracket
	whiteSpace
	return a


equAttr = do
	e <- equ
	optional (char ';' <|> char ',')
	whiteSpace
	return e


equ = do
	a <- ident
	equalsChar
	b <- ident
	return (a,b)


subgraph = do
	optional (caseInsensitiveString "subgraph" >> optional ident)
	openBrace
	(n,e) <- stmtList ([],[])
	closeBrace
	whiteSpace
	return (n,e)


openBrace = char '{' >> whiteSpace >> return '{'
closeBrace = char '}' >> whiteSpace >> return '}'
openBracket = char '[' >> whiteSpace >> return '['
closeBracket = char ']' >> whiteSpace >> return ']'
equalsChar = char '='


caseInsensitiveChar c = char (toLower c) <|> char (toUpper c)
caseInsensitiveString s = mapM caseInsensitiveChar s


whiteSpace = many (oneOf " \t")

