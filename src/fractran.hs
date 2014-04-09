
import System.Environment( getArgs )
import Control.Exception( ErrorCall(..), Handler(..), catches )
import Fractran.Parser
import Fractran.Interpreter




usageString :: String
usageString = "Usage: fractran <program file>"



program :: IO ()
program = do
	args <- getArgs
	fileContents <- if (length args /= 1)
		            then error usageString
		            else readFile (head args)

	case (parseFractran fileContents) of
		Left x -> putStrLn (show x)
		Right x -> putStrLn (show (fractran x))



main = catches program
               [ Handler ((\e -> putStrLn . show $ e) :: ErrorCall -> IO ()) ]

