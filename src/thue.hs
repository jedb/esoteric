
import System.Environment( getArgs )
import System.Console.GetOpt
import Control.Exception( ErrorCall(..), Handler(..), catches )
import Thue.Parser
import Thue.Interpreter



data Flag = LeftInterpret | RightInterpret | RandomInterpret | Version1 | Version2a
    deriving (Eq)




options :: [OptDescr Flag]
options =
    [ Option ['l'] ["left"] (NoArg LeftInterpret) "Apply rules in a list from left to right"
    , Option ['r'] ["right"] (NoArg RightInterpret) "Apply rules in a list from right to left"
    , Option ['a'] ["random"] (NoArg RandomInterpret) "Apply rules randomly"
    , Option ['1'] ["ver1"] (NoArg Version1) "Use version 1 parser"
    , Option ['2'] ["ver2a"] (NoArg Version2a) "Use version 2a parser, with quoted strings and escaped characters" ]



usageString :: String
usageString = "Usage: thue [OPTION..] <program file>"



program :: IO ()
program = do
    args <- getArgs
    let (actions, nonOptions, errors) = getOpt Permute options args

        parse = if (Version2a `elem` actions) then parseThue2a else parseThue

        order = if (LeftInterpret `elem` actions)
                then Just First
                else if (RightInterpret `elem` actions) then Just Last else Nothing

    fileContents <- if (length nonOptions /= 1)
                    then error usageString
                    else readFile (head nonOptions)

    case (parse fileContents) of
        Left x -> putStrLn (show x)
        Right x -> (thue x order) >>= (putStrLn . show)



main = catches program
               [ Handler ((\e -> putStrLn . show $ e) :: ErrorCall -> IO ()) ]

