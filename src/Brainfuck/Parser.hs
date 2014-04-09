module Brainfuck.Parser (
    BrainfuckProgram,
    BrainCom(..),

	parseBrainfuck
    ) where

import Control.Applicative( some )
import Text.ParserCombinators.Parsec



type BrainfuckProgram = [BrainCom]


data BrainCom = R | L | Inc | Dec | Out | In | OpenLoop | CloseLoop
    deriving (Show, Eq)




parseBrainfuck :: String -> Either ParseError BrainfuckProgram
parseBrainfuck = parse brainfuck "error"




brainfuck = do
	many commentChar
	bs <- many fuck
	eof
	return . concat $ bs


fuck =  (brainCommand >>= return . (:[]))
    <|> loop
    <?> "brainfuck command"


loop = do
	char '['
	many commentChar
	bs <- many brainCommand
	char ']'
	many commentChar
	return . concat $ [[OpenLoop],bs,[CloseLoop]]


brainCommand = do { b <- brainCom; many commentChar; return b }


brainCom =  (char '>' >> return R)
        <|> (char '<' >> return L)
        <|> (char '+' >> return Inc)
        <|> (char '-' >> return Dec)
        <|> (char '.' >> return Out)
        <|> (char ',' >> return In)
        <?> "brainfuck command"


commentChar = noneOf "><+-.,[]"

