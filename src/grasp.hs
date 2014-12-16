
import System.Environment( getArgs )
import Control.Exception( ErrorCall(..), Handler(..), catches )
import Grasp.Parser
import Grasp.Interpreter




usageString :: String
usageString = "Usage: grasp <program file>"



program :: IO ()
program = do
    args <- getArgs
    fileContents <- if (length args /= 1)
                    then error usageString
                    else readFile (head args)
    
    case (parseGrasp fileContents) of
        Left x -> putStrLn (show x)
        Right x -> grasp x >> return ()



main = catches program
               [ Handler ((\e -> putStrLn . show $ e) :: ErrorCall -> IO ()) ]

