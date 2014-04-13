module Thue.Parser (
	ThueProgram(..),
	ThueRule(..),
	ThueState,
	ThueChar(..),

	parseThue,
	
	tCh,
	tLit,
	tStr,
	tLitStr,
	fromThueState
	) where

import Control.Applicative( some )
import Text.ParserCombinators.Parsec




data ThueProgram = ThueProgram  { thueRules :: [ThueRule]
                                , thueInitialState :: ThueState }
    deriving (Show, Eq)


data ThueRule  =  ThueRule  { original :: ThueState
                            , replacement :: ThueState }
    deriving (Show, Eq)


type ThueState = [ThueChar]


data ThueChar = TChar { tChar :: Char }
              | TLit { tChar :: Char }
    deriving (Show, Eq)




parseThue :: String -> Either ParseError ThueProgram
parseThue = parse thue "error"



--parseThue2a :: String -> Either ParseError Thue2aProgram
--parseThue2a = parse thue2a "error"



tCh :: Char -> ThueChar
tCh = TChar

tLit :: Char -> ThueChar
tLit = TLit

tStr :: String -> ThueState
tStr = map TChar

tLitStr :: String -> ThueState
tLitStr = map TLit

fromThueState :: ThueState -> String
fromThueState = map tChar




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


ruleState = some ruleStateChar


ruleStateChar  =  (noneOf "\n\r:" >>= return . TChar)
	          <|> try (char ':' >> notFollowedBy (string ":=") >> return (TChar ':'))
	          <?> "state character"


state = many stateChar


stateChar  =  (noneOf "\n\r" >>= return . TChar)
          <?> "state character"


whiteSpace = many (oneOf "\t ")


eol  =  try (string "\r\n")
    <|> try (string "\n\r")
    <|> try (string "\r")
    <|> try (string "\n")
    <?> "end of line"
	
