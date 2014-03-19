module Fractran.Test (
	parserTests,
	interpreterTests,
	tests
    ) where


import Test.HUnit
import Text.Parsec.Error
import Fractran.Parser
import Fractran.Interpreter
import Fractran.Example


instance Eq Text.Parsec.Error.ParseError




parser0 = (Right (FractranProgram [] 2)) ~=? (parseFractran "2")

parser1 = (Right (FractranProgram [] 2)) ~=? (parseFractran "2\n")

parser2 = (Right (FractranProgram [] 2)) ~=? (parseFractran "\n2")

parser3 = (Right (FractranProgram [(1,2)] 2)) ~=? (parseFractran "2 1/2")

parser4 = (Right (FractranProgram [(2,3)] 3)) ~=? (parseFractran "3\n \n2/3\n")



interpreter0 = [108,162,243] ~=? (fractran (FractranProgram addition 108))

interpreter1 = [2,15,825,725,1925,2275,425,390,330,290,770,910,170,156,132,116,308,364,68,4] ~=? (take 20 (fractran prime2))

interpreter2 = [5] ~=? (fractran (FractranProgram addition 5))



parserTests :: Test
parserTests = TestList [parser0, parser1,parser2, parser3, parser4]



interpreterTests :: Test
interpreterTests = TestList [interpreter0, interpreter1, interpreter2]



tests :: Test
tests = case (parserTests, interpreterTests) of
	(TestList a, TestList b) -> TestList (a ++ b)

