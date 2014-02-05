module Parser (
	FractranProgram(..),

	parseFractran
	) where


import Text.Combinators.Parsec



data FractranProgram = FractranProgram { fractions :: [(Int,Int)]
                                       , initialValue :: Int }
    deriving (Show)



parseFractran :: String -> Either ParseError FractranProgram
parseFractran = parse fractran "error"




fractran = do
	f <- many intPair
	v <- initVal
	eof
	return (FractranProgram f v)


intPair = do
	whiteSpace
	n <- wholeNumber
	slash
	d <- wholeNumber
	return (n,d)


slash = char '/'


initVal = do
	whiteSpace
	v <- wholeNumber
	whiteSpace
	return v


wholeNumber =


whiteSpace = many (oneOf "\t\n\r ")

