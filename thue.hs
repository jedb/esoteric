
import System.Environment( getArgs )
import Data.Typeable
import qualified Control.Exception
import Thue.Parser
import Thue.Interpreter


data ThueException = ThueException { errString :: String }
    deriving (Show, Typeable)

instance Control.Exception.Exception ThueException



usageString :: String
usageString = "Usage: thue <program file>"



program :: IO ()
program = do
	args <- getArgs
	fileContents <- if (length args /= 1)
		            then Control.Exception.throw (ThueException usageString)
		            else readFile (head args)

	case (parseThue fileContents) of
		Left x -> putStrLn (show x)
		Right x -> (thue x) >>= (putStrLn . show)



main = Control.Exception.catch program handler
        where
        	handler :: ThueException -> IO ()
        	handler err = putStrLn (errString err)

