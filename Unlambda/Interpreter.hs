module Unlambda.Interpreter (
    unlambda
    ) where


import System.IO.Error
import Control.Monad.Trans.Cont
import Control.Monad.IO.Class
import Data.Maybe
import Unlambda.Parser
import Unlambda.Monad




unlambda :: UnlambdaTerm -> IO UnlambdaTerm
unlambda term = getResult $ eval term



eval :: UnlambdaTerm -> ULM UnlambdaTerm
eval term =
    case term of
        App f x -> do
            t <- eval f
            apply t x
        _ -> return term



apply :: UnlambdaTerm -> UnlambdaTerm -> ULM UnlambdaTerm
apply firstTerm secondTerm =
    case firstTerm of
        K -> eval secondTerm >>= return . Kpartial

        Kpartial x -> eval secondTerm >> return x

        S -> eval secondTerm >>= return . Spartial

        Spartial x -> eval secondTerm >>= return . (Sapp x)

        Sapp x y -> do
            z <- eval secondTerm
            eval (App (App x z) (App y z))

        I -> eval secondTerm

        V -> eval secondTerm >> return V

        C -> callCC $ \cont -> eval (App secondTerm (Continuation cont))

        Continuation cont -> eval secondTerm >>= cont

        D -> return (Promise secondTerm)

        Promise x -> eval secondTerm >>= eval . (App x)

        Dot c -> do
            t <- eval secondTerm
            liftIO (putChar c)
            return t

        R -> do
            t <- eval secondTerm
            liftIO (putChar '\n')
            return t

        E -> eval secondTerm >>= doExit

        Reed -> do
            t <- eval secondTerm
            ch <- liftIO (catchIOError (getChar >>= return . Just) (\e -> return Nothing))
            setCurChar ch
            if (isNothing ch) then eval (App t V) else eval (App t I)

        Bar -> do
            t <- eval secondTerm
            ch <- getCurChar
            if (isNothing ch) then eval (App t V) else eval (App t I)

        Compare c -> do
            t <- eval secondTerm
            ch <- getCurChar
            if (ch /= Just c) then eval (App t V) else eval (App t I)

