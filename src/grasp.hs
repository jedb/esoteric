

import qualified System.Environment as Env

import System.Console.GetOpt( OptDescr )
import qualified System.Console.GetOpt as GetOpt

import Control.Exception( ErrorCall(..), Handler(..), catches )

import qualified Control.Monad as Monad

import Grasp.Parser
import Grasp.Interpreter




data Flag = Testing
    deriving (Eq, Show)




usageString :: String
usageString = "Usage: grasp [--testing] <program file>"



options :: [OptDescr Flag]
options =
	[ GetOpt.Option ['t'] ["testing"] (GetOpt.NoArg Testing) "Enables before and after showing of program graph" ]



program :: IO ()
program = do
    (options, arguments, errors) <- Env.getArgs >>= return . GetOpt.getOpt GetOpt.Permute options
    
    Monad.when (length options > 1) (error usageString)
    Monad.when (length arguments /= 1) (error usageString)
    Monad.when (length errors /= 0) (error usageString)

    fileContents <- readFile (head arguments)

    case (parseGrasp fileContents) of
        Left x -> putStrLn (show x)
        Right x -> if (Testing `elem` options)
        	        then do
        	        	putStrLn "Input:"
        	        	putStrLn (show x)
        	        	y <- grasp x
        	        	putStrLn "\nOutput:"
        	        	putStrLn (show y)
        	        else grasp x >> return ()



main = catches program
               [ Handler ((\e -> putStrLn . show $ e) :: ErrorCall -> IO ()) ]

