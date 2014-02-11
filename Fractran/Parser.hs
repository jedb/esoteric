module Fractran.Parser (
    FractranProgram(..),

    parseFractran
    ) where


import Control.Applicative( some )
import Text.ParserCombinators.Parsec



data FractranProgram = FractranProgram { fractions :: [(Int,Int)]
                                       , initialValue :: Int }
    deriving (Show, Eq)




parseFractran :: String -> Either ParseError FractranProgram
parseFractran = parse fractran "error"




fractran = do
    whiteSpace
    value <- initVal
    fractionList <- many intPair
    eof
    return (FractranProgram fractionList value)


intPair = do
    numerator <- wholeNumber
    slash
    denominator <- positiveNumber
    whiteSpace
    return (numerator,denominator)


slash = char '/'


initVal = do
    value <- wholeNumber
    whiteSpace
    return value


wholeNumber = do
    value <- some digit
    return (read value)


positiveNumber = do
    firstDigit <- nonZeroDigit
    rest <- many digit
    return (read (firstDigit:rest))


nonZeroDigit = oneOf "123456789"


whiteSpace = many (oneOf "\t\n\r ")

