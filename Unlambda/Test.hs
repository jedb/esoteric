module Unlambda.Test (
    parserTests,
    tests
    ) where


import Test.HUnit
import Text.Parsec.Error
import Unlambda.Types
import Unlambda.Parser
import Unlambda.Interpreter


instance Eq Text.Parsec.Error.ParseError




parser0 = TestCase (assertEqual ""
                                (Right S)
                                (parseUnlambda "s"))

parser1 = TestCase (assertEqual ""
                                (Right K)
                                (parseUnlambda "k"))

parser2 = TestCase (assertEqual ""
                                (Right I)
                                (parseUnlambda "i"))

parser3 = TestCase (assertEqual ""
                                (Right V)
                                (parseUnlambda "v"))

parser4 = TestCase (assertEqual ""
                                (Right R)
                                (parseUnlambda "r"))

parser5 = TestCase (assertEqual ""
                                (Right D)
                                (parseUnlambda "d"))

parser6 = TestCase (assertEqual ""
                                (Right C)
                                (parseUnlambda "c"))

parser7 = TestCase (assertEqual ""
                                (Right E)
                                (parseUnlambda "e"))

parser8 = TestCase (assertEqual ""
                                (Right (App S K))
                                (parseUnlambda "`sk"))

parser9 = TestCase (assertEqual ""
                                (Right (Dot 'c'))
                                (parseUnlambda ".c"))

parser10 = TestCase (assertEqual ""
                                (Right (Compare '?'))
                                (parseUnlambda "??"))

parser11 = TestCase (assertEqual ""
                                (Right Bar)
                                (parseUnlambda "|"))

parser12 = TestCase (assertEqual ""
                                (Right Reed)
                                (parseUnlambda "@"))



parserTests :: Test
parserTests = TestList [parser0, parser1, parser2, parser3, parser4, parser5, parser6, parser7, parser8
                       ,parser9, parser10, parser11, parser12]



tests :: Test
tests = parserTests

