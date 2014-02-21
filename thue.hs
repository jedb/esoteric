
import System.Environment( getArgs )
import Data.Typeable
import Control.Exception( ErrorCall(..), Handler(..), catches )
import Thue.Parser
import Thue.Interpreter




usageString :: String
usageString = "Usage: thue <program file>"



program :: IO ()
program = do
	args <- getArgs
	fileContents <- if (length args /= 1)
		            then error usageString
		            else readFile (head args)

	case (parseThue fileContents) of
		Left x -> putStrLn (show x)
		Right x -> (thue x) >>= (putStrLn . show)



main = catches program
               [ Handler ((\e -> putStrLn . show $ e) :: ErrorCall -> IO ()) ]

