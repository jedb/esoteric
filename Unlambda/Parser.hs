module Unlambda.Parser (
    UnlambdaTerm(..),

	parseUnlambda
    ) where


import Control.Applicative( some )
import Text.ParserCombinators.Parsec



data UnlambdaTerm = S | K | I | V | R | D | C
                  | Dot { cha :: Char }
	              | App { func :: UnlambdaTerm
                        , arg :: UnlambdaTerm }
    deriving (Eq, Show)




parseUnlambda :: String -> Either ParseError UnlambdaTerm
parseUnlambda = parse unlambda "error"



unlambda = do
	whiteSpace
	t <- term
	eof
	return t


term  =  (try app)
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


dot = do
	char '.'
	c <- noneOf("")
	whiteSpace
	return (Dot c)


whiteSpace = many (oneOf "\t\n\r ")

