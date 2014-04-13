module Thue.Parser (
	ThueProgram(..),
	ThueRule(..),
	ThueState,
	ThueChar(..),

	ThueVersion(..),

	parseThue,
	parseThue2a,
	
	tCh,
	tLit,
	tStr,
	tLitStr,
	fromThueState
	) where

import Control.Applicative( some )
import Text.ParserCombinators.Parsec




data ThueProgram = ThueProgram  { thueRules :: [ThueRule]
                                , thueInitialState :: ThueState
                                , thueVersion :: ThueVersion }
    deriving (Show, Eq)


data ThueRule  =  ThueRule  { original :: ThueState
                            , replacement :: ThueState }
    deriving (Show, Eq)


type ThueState = [ThueChar]


data ThueChar = TChar { tChar :: Char }
              | TLit { tChar :: Char }
    deriving (Show, Eq)


data ThueVersion = Ver1 | Ver2a
    deriving (Show, Eq)




parseThue :: String -> Either ParseError ThueProgram
parseThue = parse (thue Ver1) "error"



parseThue2a :: String -> Either ParseError ThueProgram
parseThue2a = parse (thue Ver2a) "error"



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




thue ver = do
	rs <- many (rule ver)
	separatorLine
	i <- initialState ver
	eof
	return (ThueProgram rs i ver)


rule ver = do
	o <- ruleState ver
	separator
	r <- state ver
	eol
	return (ThueRule o r)


separatorLine = separator >> eol
separator  =  string "::="
          <?> "rule separator"


initialState ver = do
	s <- (state ver) `sepEndBy` eol
	return (concat s)


ruleState ver = some (ruleStateChar ver)


ruleStateChar ver =
	(noneOf "\n\r:" >>= return . TChar)
	<|> try (char ':' >> notFollowedBy (string ":=") >> return (TChar ':'))
	<?> "state character"


state ver = many (stateChar ver)


stateChar ver =
	(noneOf "\n\r" >>= return . TChar)
    <?> "state character"


eol  =  try (string "\r\n")
    <|> try (string "\n\r")
    <|> try (string "\r")
    <|> try (string "\n")
    <?> "end of line"
	
