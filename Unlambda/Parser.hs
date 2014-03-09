module Unlambda.Parser (
    UnlambdaTerm(..),

    parseUnlambda,
    parseUnlambda1
    ) where


import Control.Applicative( some )
import Data.Either
import Text.ParserCombinators.Parsec



data UnlambdaTerm = S | K | I | V | R | D | C | E | Bar | Reed
                  | Dot Char
                  | Compare Char
                  | App UnlambdaTerm UnlambdaTerm
                  | Kpartial UnlambdaTerm
                  | Spartial UnlambdaTerm
                  | Sapp UnlambdaTerm UnlambdaTerm
                  | Continuation UnlambdaTerm
                  | Promise UnlambdaTerm
    deriving (Eq, Show)




parseUnlambda :: String -> Either ParseError UnlambdaTerm
parseUnlambda input = 
    let firstPass = parse removeComments "error" input
    in case firstPass of
        Left e -> Left e
        Right o -> parse unlambda "error" o



parseUnlambda1 :: String -> Either ParseError UnlambdaTerm
parseUnlambda1 input =
    let firstPass = parse removeComments "error" input
    in case firstPass of
        Left e -> Left e
        Right o -> parse unlambda1 "error" o



removeComments = uline `sepEndBy` eol >>= (return . concat)


uline = do
    l <- many (builtin <|> (oneOf " \t" >>= return . (:[])))
    optional (char '#' >> many (noneOf "\r\n"))
    return . concat $ l


builtin  =  (oneOf "`skivrdce|@" >>= return . (:[]))
        <|> (char '.' >> anyChar >>= return . ('.':) . (:[]))
        <|> (char '?' >> anyChar >>= return . ('?':) . (:[]))
        <?> "unlambda builtin function"


eol  =  try (string "\r\n")
    <|> try (string "\n\r")
    <|> try (string "\r")
    <|> try (string "\n")
    <?> "end of line"


unlambda = do
    whiteSpace
    t <- term
    eof
    return t


unlambda1 = do
    whiteSpace
    t <- term1
    eof
    return t


term  =  (try term1)
     <|> (try e)
     <|> (try reed)
     <|> (try comp)
     <|> (try bar)
     <?> "unlambda term"


term1  =  (try app)
      <|> (try s)
      <|> (try k)
      <|> (try i)
      <|> (try v)
      <|> (try r)
      <|> (try d)
      <|> (try c)
      <|> (try dot)
      <?> "unlambda term"


app = do
    char '`'
    whiteSpace
    f <- term
    x <- term
    return (App f x)


s = char 's' >> whiteSpace >> return S
k = char 'k' >> whiteSpace >> return K
i = char 'i' >> whiteSpace >> return I
v = char 'v' >> whiteSpace >> return V
r = char 'r' >> whiteSpace >> return R
d = char 'd' >> whiteSpace >> return D
c = char 'c' >> whiteSpace >> return C
e = char 'e' >> whiteSpace >> return E
reed = char '@' >> whiteSpace >> return Reed
bar = char '|' >> whiteSpace >> return Bar


comp = do
    char '?'
    c <- anyChar
    whiteSpace
    return (Compare c)


dot = do
    char '.'
    c <- anyChar
    whiteSpace
    return (Dot c)


whiteSpace = many (oneOf "\t\n\r ")

