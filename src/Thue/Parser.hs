module Thue.Parser (
	ThueProgram(..),
	ThueRule(..),
	ThueState,

	parseThue,
	toThueState,
	fromThueState
	) where

import Control.Applicative( some )
import Text.ParserCombinators.Parsec



data ThueProgram = ThueProgram { thueRules :: [ThueRule]
                               , thueInitialState :: ThueState }
    deriving (Show, Eq)


data ThueRule = ThueRule { original :: ThueState
                         , replacement :: ThueState }
    deriving (Show, Eq)


type ThueState = String




parseThue :: String -> Either ParseError ThueProgram
parseThue = parse thue "error"



toThueState :: String -> ThueState
toThueState = id



fromThueState :: ThueState -> String
fromThueState = id




thue = do
	rs <- many rule
	separatorLine
	i <- initialState
	eof
	return (ThueProgram rs i)


rule = do
	o <- ruleState
	separator
	r <- state
	eol
	return (ThueRule o r)


separatorLine = whiteSpace >> separator >> whiteSpace >> eol
separator  =  string "::="
          <?> "rule separator"


initialState = do
	s <- state `sepEndBy` eol
	return (concat s)


ruleState = some ruleStateChar >>= return . toThueState


ruleStateChar  =  noneOf "\n\r:"
	          <|> try (char ':' >> notFollowedBy (string ":=") >> return ':')
	          <?> "state character"


state = many stateChar >>= return . toThueState


stateChar  =  noneOf "\n\r"
          <?> "state character"


whiteSpace = many (oneOf "\t ")


eol  =  try (string "\r\n")
    <|> try (string "\n\r")
    <|> try (string "\r")
    <|> try (string "\n")
    <?> "end of line"
	
