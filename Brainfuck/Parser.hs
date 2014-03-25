module Brainfuck.Parser (
	parseBrainfuck
    ) where

import Control.Applicative( some )
import Text.ParserCombinators.Parsec



type BrainfuckProgram = [BrainCom]


data BrainCom = R | L | Inc | Dec | Out | In | OpenLoop | CloseLoop
    deriving (Show, Eq)




parseBrainfuck :: String -> Either ParseError BrainfuckProgram
parseBrainfuck = parse brainfuck "error"




brainfuck = many commentChar >> many fuck


fuck = do
	f <- brainCommand
	many commentChar
	return f


brainCommand =  try (char '>' >> return R)
            <|> try (char '<' >> return L)
            <|> try (char '+' >> return Inc)
            <|> try (char '-' >> return Dec)
            <|> try (char '.' >> return Out)
            <|> try (char ',' >> return In)
            <|> try (char '[' >> return OpenLoop)
            <|> try (char ']' >> return CloseLoop)
            <?> "brainfuck command"


commentChar = noneOf "><+-.,[]"

