
import System.Environment( getArgs )
import Data.Typeable
import qualified Control.Exception
import Fractran.Parser
import Fractran.Interpreter


data FractranException = FractranException { errString :: String }
    deriving (Show, Typeable)

instance Control.Exception.Exception FractranException



usageString :: String
usageString = "Usage: fractran <program file>"



program :: IO ()
program = do
	args <- getArgs
	fileContents <- if (length args /= 1)
		            then Control.Exception.throw (FractranException usageString)
		            else readFile (head args)

	case (parseFractran fileContents) of
		Left x -> putStrLn (show x)
		Right x -> putStrLn (show (fractran x))



main = Control.Exception.catch program handler
        where
        	handler :: FractranException -> IO ()
        	handler err = putStrLn (errString err)

