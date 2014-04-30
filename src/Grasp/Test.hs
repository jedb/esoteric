module Grasp.Test (
	parserTests,
	tests
    ) where


import Test.HUnit
import Text.Parsec.Error
import Grasp.Parser


instance Eq Text.Parsec.Error.ParseError




isParseError :: Either a b -> Bool
isParseError (Left _) = True
isParseError (Right _) = False



parser0 = TestCase (assertBool "" (isParseError (parseGrasp "")) )

parser1 = TestCase (assertBool "" (isParseError (parseGrasp "digraph {")) )

parser2 = TestCase (assertBool "" (isParseError (parseGrasp "digraph { }")) )



parserTests :: Test
parserTests = TestList [parser0, parser1, parser2]



tests :: Test
tests = parserTests

