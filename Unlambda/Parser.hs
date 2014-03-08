module Unlambda.Parser (
    UnlambdaTerm(..),

	parseUnlambda
    ) where


import Control.Applicative( some )
import Text.ParserCombinators.Parsec



data UnlambdaTerm = S | K | I | V | R | D | C | E | Bar | Reed
                  | Dot { cha :: Char }
                  | Compare { cha :: Char }
	              | App { func :: UnlambdaTerm
                        , arg :: UnlambdaTerm }
                  | Kpartial { constant :: UnlambdaTerm }
                  | Spartial { func1 :: UnlambdaTerm }
                  | Sapp { func1 :: UnlambdaTerm
                         , func2 :: UnlambdaTerm }
                  | Continuation { arg :: UnlambdaTerm }
                  | Promise { arg :: UnlambdaTerm }
    deriving (Eq, Show)




parseUnlambda :: String -> Either ParseError UnlambdaTerm
parseUnlambda = parse unlambda "error"



parseUnlambda1 :: String -> Either ParseError UnlambdaTerm
parseUnlambda1 = parse unlambda1 "error"



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
	c <- noneOf("")
	whiteSpace
	return (Compare c)


dot = do
	char '.'
	c <- noneOf("")
	whiteSpace
	return (Dot c)


whiteSpace = many (oneOf "\t\n\r ")

