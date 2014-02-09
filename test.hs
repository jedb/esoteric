
import Test.HUnit
import Thue.Parser
import Fractran.Parser




fractranParser1 = TestCase (assertEqual ""
	                                    (Right (FractranProgram [] 2))
	                                    (parseFractran "2"))

fractranParser2 = TestCase (assertEqual ""
	                                    (Right (FractranProgram [] 2))
	                                    (parseFractran "2\n"))

fractranParser3 = TestCase (assertEqual ""
	                                    (Right (FractranProgram [] 2))
	                                    (parseFractran "\n2"))

fractranParser4 = TestCase (assertEqual ""
	                                    (Left x)
	                                    (parseFractran "2/3"))



main = do
	putStrLn "Fractran Parser"
	runTestTT $ TestList [fractranParser1, fractranParser2, fractranParser3, fractranParser4]

