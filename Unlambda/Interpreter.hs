module Unlambda.Interpreter (
    unlambda
    ) where


import System.IO.Error
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class
import Unlambda.Parser



type ULM a = ContT UnlambdaTerm (StateT UnlambdaState IO) a

data UnlambdaState = UnlambdaState { exit :: UnlambdaTerm -> ULM UnlambdaTerm
                                   , curChar :: Maybe Char }




unlambda :: UnlambdaTerm -> IO UnlambdaTerm
unlambda term = (`getResult` return) (callCC $ \cont -> setExit cont >> setCurChar Nothing >> eval term)



getResult :: (Monad m) => ULM a -> (a -> m a) -> m a
getResult m f = f . runStateT . runContT $ m



setExit :: (UnlambdaTerm -> ULM UnlambdaTerm) -> ULM ()
setExit cont = do
    state <- lift get
    (lift put) (state { exit = cont })
    return



doExit :: UnlambdaTerm -> ULM UnlambdaTerm
doExit term = do
    state <- lift get
    (exit state) term
    return



setCurChar :: Maybe Char -> ULM ()
setCurChar ch = do
    state <- lift get
    (lift put) (state { curChar = ch })
    return



getCurChar :: ULM (Maybe Char)
getCurChar = do
    state <- lift get
    return (curChar state)



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

        Reed -> return I --do
            --t <- eval ch secondTerm
            --catchIOError (do
            --    ch' <- getChar
            --    eval (Just ch') (App t I)
            --    ) (\e -> eval Nothing (App t V))

        Bar -> return I --do
            --t <- eval ch secondTerm
            --case ch of
            --    Just x -> eval ch (App t (Dot x))
            --    Nothing -> eval ch (App t V)

        Compare c -> return I --do
            --t <- eval ch secondTerm
            --case (ch == Just c) of
            --    True -> eval ch (App t I)
            --    False -> eval ch (App t V)

