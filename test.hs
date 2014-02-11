
import Test.HUnit
import Text.Parsec.Error
import Thue.Parser
import Fractran.Parser




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



main = do
	putStrLn "Fractran Parser"
	runTestTT $ TestList [fractranParser0, fractranParser1, fractranParser2, fractranParser3, fractranParser4]

	putStrLn "\nThue Parser"
	runTestTT $ TestList [thueParser0]

