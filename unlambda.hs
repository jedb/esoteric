
import System.Environment( getArgs )
import Control.Exception( ErrorCall(..), Handler(..), catches )
import Unlambda.Parser
import Unlambda.Interpreter




usageString :: String
usageString = "Usage: unlambda <program file>"



program :: IO ()
program = do
    args <- getArgs
    fileContents <- if (length args /= 1)
                    then error usageString
                    else readFile (head args)

    case (parseUnlambda fileContents) of
        Left x -> putStrLn (show x)
        Right x -> unlambda x >>= putStrLn . show



main = catches program
               [ Handler ((\e -> putStrLn . show $ e) :: ErrorCall -> IO ()) ]

