
import System.Environment( getArgs )
import Control.Exception( ErrorCall(..), Handler(..), catches )
import Brainfuck.Parser
import Brainfuck.Interpreter




usageString :: String
usageString = "Usage: brainfuck <program file>"



program :: IO ()
program = do
	args <- getArgs
	fileContents <- if (length args /= 1)
		            then error usageString
		            else readFile (head args)

	case (parseBrainfuck fileContents) of
		Left x -> putStrLn (show x)
		Right x -> brainfuck x >> return ()



main = catches program
               [ Handler ((\e -> putStrLn . show $ e) :: ErrorCall -> IO ()) ]

