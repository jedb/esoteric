module Thue.Test (
	parserTests,
	extractInfixTests,
	tests
    ) where


import Test.HUnit
import Text.Parsec.Error
import Thue.Parser
import Thue.Interpreter


instance Eq Text.Parsec.Error.ParseError




parser0 = TestCase (assertEqual ""
                                (Right (ThueProgram [ThueRule "a" "b"] "a"))
                                (parseThue "a::=b\n::=\na"))

parser1 = TestCase (assertEqual ""
                                (Right (ThueProgram [] "b"))
                                (parseThue "::=\nb"))



extractInfix0 = TestCase (assertEqual ""
                                    Nothing
                                    (extractInfix [1,2] [3,4,5]))

extractInfix1 = TestCase (assertEqual ""
                                    (Just ([1,2],[5,6]))
                                    (extractInfix [3,4] [1,2,3,4,5,6]))

extractInfix2 = TestCase (assertEqual ""
                                    (Just ([],[3,4]))
                                    (extractInfix [0,1,2] [0,1,2,3,4]))

extractInfix3 = TestCase (assertEqual ""
                                    (Just ([1],[]))
                                    (extractInfix [2,3] [1,2,3]))

extractInfix4 = TestCase (assertEqual ""
                                    (Just ([],[1]))
                                    (extractInfix [] [1]))

extractInfix5 = TestCase (assertEqual ""
                                    (Just ("before","after"))
                                    (extractInfix "middle" "beforemiddleafter"))



parserTests :: Test
parserTests = TestList [parser0, parser1]



extractInfixTests :: Test
extractInfixTests = TestList [extractInfix0, extractInfix1, extractInfix2, extractInfix3, extractInfix4, extractInfix5]



tests :: Test
tests = case (parserTests, extractInfixTests) of
	(TestList a, TestList b) -> TestList (a ++ b)

