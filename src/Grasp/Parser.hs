module Grasp.Parser (
    parseGrasp
    ) where




import Control.Applicative( some )
import Data.Char( toLower, toUpper )
import Text.ParserCombinators.Parsec

import Grasp.Types.GNode( GNode )
import qualified Grasp.Types.GNode as GN

import Grasp.Types.GEdge( GEdge )
import qualified Grasp.Types.GEdge as GE

import Grasp.Types.Instruction( Instruction )
import qualified Grasp.Types.Instruction as IN

import Grasp.Types.EdgeLabel( EdgeLabel )
import qualified Grasp.Types.EdgeLabel as EL




parseGrasp :: String -> Either ParseError ([GNode],[GEdge])
parseGrasp input =
    parse removeComments "error" input >>=
    parse graspDOT "error"




-- removes comments but otherwise leaves input unchanged

removeComments = gline `sepEndBy` eol >>= return . concat . (map (++ "\n"))


eol  =  try (string "\r\n")
    <|> try (string "\n\r")
    <|> try (string "\n")
    <|> try (string "\r")
    <?> "end of line"


gline = many thing >>= return . concat


thing = try (some (noneOf "\r\n\"/#"))
    <|> try quotedNonComment
    <|> try singleLineComment
    <|> try multiLineComment
    <|> ((noneOf "\r\n") >>= return . (:[]))


quotedNonComment = do
    char '\"'
    x <- many (noneOf "\r\n")
    char '\"'
    return ("\"" ++ x ++ "\"")
    

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
    whiteSpace
    optional strict
    graphType
    optional ident
    openBrace
    (n,e) <- stmtList ([],[])
    closeBrace
    whiteSpace
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
    <|> return (reverse n, reverse e)


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
    return (GN.mk (n, IN.mk a))


edge = do
    a <- ident
    edgeOp
    b <- ident
    c <- attrList
    optional (char ';')
    whiteSpace
    return (GE.mk (a,b, EL.mk c))


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


whiteSpace = many (oneOf " \t\r\n")

