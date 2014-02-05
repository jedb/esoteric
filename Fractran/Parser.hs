module Parser (
    FractranProgram(..),

    parseFractran
    ) where


import Control.Applicative( some )
import Text.ParserCombinators.Parsec



data FractranProgram = FractranProgram { fractions :: [(Int,Int)]
                                       , initialValue :: Int }
    deriving (Show)




parseFractran :: String -> Either ParseError FractranProgram
parseFractran = parse fractran "error"




fractran = do
    value <- initVal
    fractionList <- many intPair
    whiteSpace
    eof
    return (FractranProgram fractionList value)


intPair = do
    whiteSpace
    numerator <- wholeNumber
    slash
    denominator <- positiveNumber
    return (numerator,denominator)


slash = char '/'


initVal = do
    whiteSpace
    value <- wholeNumber
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

