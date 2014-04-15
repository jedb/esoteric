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


ruleState Ver2a = some ruleStatePart >>= return . concat
ruleState Ver1 = some (ruleStateChar Ver1) >>= return . tStr


ruleStatePart =
	(try (some (ruleStateChar Ver2a) >>= return . tStr))
	<|> quoteString
	<?> "thue state"


ruleStateChar Ver2a = try escapeChar <|> (noneOf "\\\n\r\":") <|> colon <?> "state character"
ruleStateChar Ver1 = (noneOf "\n\r:") <|> colon <?> "state character"
colon = try (char ':' >> notFollowedBy (string ":=") >> return ':')


state Ver2a = many statePart >>= return . concat
state Ver1 = many (stateChar Ver1) >>= return . tStr


statePart =
	(try (some (stateChar Ver2a) >>= return . tStr))
	<|> quoteString
	<?> "thue state"


stateChar Ver2a = (try escapeChar) <|> (noneOf "\\\n\r\"") <?> "state character"
stateChar Ver1 = (noneOf "\n\r") <?> "state character"


escapeChar = char '\\' >> charCode
charCode =
	(char 'n' >> return '\n')
	<|> (char '"')
	<|> (char 'E' >> char 'O' >> char 'T' >> return '\EOT')
	<|> (char ':')
	<|> (char 'r' >> return '\r')
	<|> (char '\\')


quoteString = do
	char '"'
	str <- some (stateChar Ver2a)
	char '"'
	return (tLitStr str)


eol  =  try (string "\r\n")
    <|> try (string "\n\r")
    <|> try (string "\r")
    <|> try (string "\n")
    <?> "end of line"
	
