module Grasp.Parser (
    parseGrasp
    ) where


import Control.Applicative( some )
import Data.Char( toLower, toUpper )
import Text.ParserCombinators.Parsec
import Grasp.Types( Instruction(..), EdgeLabel(..), GNode(..), GEdge(..) )




parseGrasp :: String -> Either ParseError ([GNode],[GEdge])
parseGrasp input =
    parse removeComments "error" input >>=
    parse graspDOT "error"




-- removes comments but otherwise leaves input unchanged

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
    string "*/"
    return ""



-- parses a DOT graph language file into the data for a grasp program

graspDOT = do
    optional strict
    graphType
    optional ident
    openBrace
    (n,e) <- stmtList ([],[])
    closeBrace
    many blankLine
    eof
    return (n,e)


strict = caseInsensitiveString "strict" >>= (\x -> whiteSpace >> return x)


graphType = try (caseInsensitiveString "digraph" >>= (\x -> whiteSpace >> return x)) <?> "digraph"


ident = ((try alphaNumString)
    <|>  (try numeral)
    <|>  (try quotedString)) >>= (\x -> whiteSpace >> return x)
    <?> "ID"


stmtList (n,e) =
        try (whiteSpace >> node >>= (\x -> stmtList (x:n,e)) )
    <|> try (whiteSpace >> edge >>= (\x -> stmtList (n,x:e)) )
    <|> try (whiteSpace >> attr >> stmtList (n,e))
    <|> try (whiteSpace >> subgraph >>= (\(x,y) -> stmtList ((reverse x) ++ n, (reverse y) ++ e)) )
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
    optional (char ';')
    whiteSpace
    return (GNode (n, Instruction a))


edge = do
    a <- ident
    edgeOp
    b <- ident
    c <- attrList
    optional (char ';')
    whiteSpace
    return (GEdge (a,b, EdgeLabel c))


edgeOp = string "->" >> whiteSpace >> return "->"


attr = attrType >> attrList >> optional (char ';') >> whiteSpace
attrType = (caseInsensitiveString "graph"
        <|> caseInsensitiveString "node"
        <|> caseInsensitiveString "edge") >>= (\x -> whiteSpace >> return x)


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
    optional (char ';')
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

