module Unlambda.Interpreter (
    unlambda
    ) where


import System.IO.Error
import Control.Exception( Exception(..), Handler(..), throw, catches )
import Control.Monad.Trans.Cont
import Control.Monad.IO.Class
import Data.Typeable
import Unlambda.Parser



data UnlambdaException = UnlambdaException { endTerm :: UnlambdaTerm }
    deriving (Show, Typeable)

instance Control.Exception.Exception UnlambdaException




unlambda :: UnlambdaTerm -> IO UnlambdaTerm
unlambda term =
    catches ((`runContT` return) $ eval term)
            [ Handler ((\e -> return (endTerm e)) :: UnlambdaException -> IO UnlambdaTerm) ]



eval :: UnlambdaTerm -> ContT UnlambdaTerm IO UnlambdaTerm
eval term =
    case term of
        App f x -> do
            t <- eval f
            apply t x
        _ -> return term



apply :: UnlambdaTerm -> UnlambdaTerm -> ContT UnlambdaTerm IO UnlambdaTerm
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

        E -> eval secondTerm >>= throw . UnlambdaException

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

