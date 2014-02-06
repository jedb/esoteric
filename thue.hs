
import Text.Combinators.Parsec



data ThueProgram = ThueProgram { thueRules :: [Rule]
                               , thueInitialState :: State }
    deriving (Show)


data Rule = Rule { original :: State
                 , replacement :: State }
    deriving (Show)


type State = String




parseThue :: String -> Either ParseError ThueProgram
parseThue = parse thue "error"




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
	return (Rule o r)


separatorLine = whiteSpace >> separator >> whiteSpace >> eol
separator = string "::="


initialState = do
	s <- state `sepEndBy` eol
	return (concat s)


ruleState = many ruleStateChar


ruleStateChar  =  noneOf "\n\r:"
	          <|> try (do { char ':'; notFollowedBy (string ":="); return ':'})


state = many stateChar


stateChar = noneOf "\n\r"


whiteSpace = many (oneOf "\t ")


eol  =  try (string "\r\n")
    <|> try (string "\n\r")
    <|> try (string "\r")
    <|> try (string "\n")
	
