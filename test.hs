
import Test.HUnit
import Text.Parsec.Error

import Thue.Parser
import Fractran.Parser
import Thue.Interpreter
import Fractran.Interpreter
import Fractran.Example




instance Eq Text.Parsec.Error.ParseError



fractranParser0 = TestCase (assertEqual ""
                                        (Right (FractranProgram [] 2))
                                        (parseFractran "2"))

fractranParser1 = TestCase (assertEqual ""
                                        (Right (FractranProgram [] 2))
                                        (parseFractran "2\n"))

fractranParser2 = TestCase (assertEqual ""
                                        (Right (FractranProgram [] 2))
                                        (parseFractran "\n2"))

fractranParser3 = TestCase (assertEqual ""
                                        (Right (FractranProgram [(1,2)] 2))
                                        (parseFractran "2 1/2"))

fractranParser4 = TestCase (assertEqual ""
                                        (Right (FractranProgram [(2,3)] 3))
                                        (parseFractran "3\n \n2/3\n"))



thueParser0 = TestCase (assertEqual ""
                                    (Right (ThueProgram [ThueRule "a" "b"] "a"))
                                    (parseThue "a::=b\n::=\na"))

thueParser1 = TestCase (assertEqual ""
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



fractranInterpreter0 = TestCase (assertEqual ""
                                             [108,162,243]
                                             (fractran (FractranProgram addition 108)))

fractranInterpreter1 = TestCase (assertEqual ""
                                             [2,15,825,725,1925,2275,425,390,330,290,770,910,170,156,132,116,308,364,68,4]
                                             (take 20 (fractran prime2)))

fractranInterpreter2 = TestCase (assertEqual ""
                                             [5]
                                             (fractran (FractranProgram addition 5)))



main = do
    putStrLn "Fractran Parser"
    runTestTT $ TestList [fractranParser0, fractranParser1, fractranParser2, fractranParser3, fractranParser4]

    putStrLn "\nFractran Interpreter"
    runTestTT $ TestList [fractranInterpreter0, fractranInterpreter1, fractranInterpreter2]
    
    putStrLn "\nThue Parser"
    runTestTT $ TestList [thueParser0, thueParser1]

    putStrLn "\nextractInfix"
    runTestTT $ TestList [extractInfix0, extractInfix1, extractInfix2, extractInfix3, extractInfix4, extractInfix5]

