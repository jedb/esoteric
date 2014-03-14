module Unlambda.Types (
	ULM,
	UnlambdaTerm(..),

    getResult,
	doExit,
	setCurChar,
	getCurChar
	) where


import Control.Exception
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class
import Data.Typeable




data UnlambdaException = UnlambdaException { exitTerm :: UnlambdaTerm }
    deriving (Show, Typeable)

instance Exception UnlambdaException




type ULM a = ContT UnlambdaTerm (StateT (Maybe Char) IO) a




data UnlambdaTerm = S | K | I | V | R | D | C | E | Bar | Reed
                  | Dot Char
                  | Compare Char
                  | App UnlambdaTerm UnlambdaTerm
                  | Kpartial UnlambdaTerm
                  | Spartial UnlambdaTerm
                  | Sapp UnlambdaTerm UnlambdaTerm
                  | Promise UnlambdaTerm
                  | Continuation (UnlambdaTerm -> ULM UnlambdaTerm)


instance Eq UnlambdaTerm where
    S == S   =   True
    K == K   =   True
    I == I   =   True
    V == V   =   True
    R == R   =   True
    D == D   =   True
    C == C   =   True
    E == E   =   True
    Bar == Bar   =   True
    Reed == Reed   =   True
    Dot x == Dot y   =   x == y
    Compare x == Compare y   =   x == y
    App a b == App x y   =   a == x && b == y
    Kpartial x == Kpartial y   =   x == y
    Spartial x == Spartial y   =   x == y
    Sapp a b == Sapp x y   =   a == x && b == y
    Promise x == Promise y   =   x == y
    _ == _   =   False


instance Show UnlambdaTerm where
    show S = "s"
    show K = "k"
    show I = "i"
    show V = "v"
    show R = "r"
    show D = "d"
    show C = "c"
    show E = "e"
    show Bar = "|"
    show Reed = "@"
    show (Dot x) = ['.', x]
    show (Compare x) = ['?', x]
    show (App x y) = "`" ++ (show x) ++ (show y)
    show (Kpartial x) = "`k" ++ (show x)
    show (Spartial x) = "`s" ++ (show x)
    show (Sapp x y) = "``s" ++ (show x) ++ (show y)
    show (Promise x) = "`d" ++ (show x)
    show (Continuation _) = "<cont>"




getResult :: ULM UnlambdaTerm -> IO UnlambdaTerm
getResult m = catches (liftIO ((`evalStateT` Nothing) . (`runContT` return) $ m))
                      [ Handler ((\e -> return (exitTerm e)) :: UnlambdaException -> IO UnlambdaTerm) ]



doExit :: UnlambdaTerm -> ULM UnlambdaTerm
doExit term = throw (UnlambdaException term)



setCurChar :: Maybe Char -> ULM ()
setCurChar x = lift (put x)



getCurChar :: ULM (Maybe Char)
getCurChar = lift (get)

