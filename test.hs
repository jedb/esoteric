
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

--fractranParser3 = TestCase (assertEqual ""
--	                                    (Left _)
--	                                    (parseFractran "2/3"))



main = do
	putStrLn "Fractran Parser"
	runTestTT $ TestList [fractranParser0, fractranParser1, fractranParser2]

