
import Test.HUnit
import qualified Fractran.Test
import qualified Thue.Test
import qualified Unlambda.Test
import qualified Grasp.Test




main = do
    putStrLn "Fractran Parser"
    runTestTT Fractran.Test.parserTests

    putStrLn "\nFractran Interpreter"
    runTestTT Fractran.Test.interpreterTests

    putStrLn "\nThue Parser"
    runTestTT Thue.Test.parserTests

    putStrLn "\nextractInfix"
    runTestTT Thue.Test.extractInfixTests

    putStrLn "\nUnlambda Parser"
    runTestTT Unlambda.Test.parserTests

    putStrLn "\nUnlambda Interpreter"
    tests <- Unlambda.Test.interpreterTests
    runTestTT tests

    putStrLn "\nGrasp Parser"
    runTestTT Grasp.Test.parserTests

