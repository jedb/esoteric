module Parser (
	FractranProgram(..),

	parseFractran
	) where


import Text.Combinators.Parsec



data FractranProgram = FractranProgram { fractions :: [(Int,Int)] }



parseFractran :: String -> Either ParseError FractranProgram
parseFractran = parse fractran "error"


