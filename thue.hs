
import Text.Combinators.Parsec



data ThueProgram = ThueProgram { substitutionRules :: [Rule]
                               , initialState :: State }
    deriving (Show)


data Rule = Rule { original :: State
                 , replacement :: State }
    deriving (Show)


type State = String




parseThue :: String -> Either ParseError ThueProgram
parseThue = parse thue "error"




thue = do
	rs <- many ruleLine
	separatorLine
	i <- stateLine
	return (ThueProgram rs i)


ruleLine = do
	r <- rule
	eol
	return r


rule = do
	o <- state
	separator
	r <- state
	return (Rule o r)


separatorLine = separator >> eol
separator = string "::="


stateLine = do
	s <- state
	eol
	return s


state =
	

