module Unlambda.Interpreter (
    unlambda
    ) where


import System.IO.Error
import Control.Exception( Exception(..), Handler(..), throw, catches )
import Data.Typeable
import Unlambda.Parser



data UnlambdaException = UnlambdaException { endTerm :: UnlambdaTerm }
    deriving (Show, Typeable)

instance Control.Exception.Exception UnlambdaException




unlambda :: UnlambdaTerm -> IO UnlambdaTerm
unlambda term = 
    catches (eval Nothing term)
            [ Handler ((\e -> return (endTerm e)) :: UnlambdaException -> IO UnlambdaTerm) ]



eval :: Maybe Char -> UnlambdaTerm -> IO UnlambdaTerm
eval ch term =
    case term of
        App f x -> do
            t <- eval ch f
            apply ch t x
        _ -> return term



apply :: Maybe Char -> UnlambdaTerm -> UnlambdaTerm -> IO UnlambdaTerm
apply ch firstTerm secondTerm =
    case firstTerm of
        K -> eval ch secondTerm >>= return . Kpartial

        Kpartial x -> eval ch secondTerm >> return x

        S -> eval ch secondTerm >>= return . Spartial

        Spartial x -> eval ch secondTerm >>= return . (Sapp x)

        Sapp x y -> do
            z <- eval ch secondTerm
            result <- eval ch (App (App x z) (App y z))
            return result

        I -> eval ch secondTerm

        V -> eval ch secondTerm >> return V

        C -> return I --placeholder

        D -> return (Promise secondTerm)

        Promise x -> do
            y <- eval ch secondTerm
            result <- eval ch (App x y)
            return result

        Dot c -> putChar c >> eval ch secondTerm

        R -> putChar '\n' >> eval ch secondTerm

        E -> do
            t <- eval ch secondTerm
            throw (UnlambdaException t)

        Reed -> do
            t <- eval ch secondTerm
            catchIOError (do
                ch' <- getChar
                eval (Just ch') (App t I)
                ) (\e -> eval Nothing (App t V))

        Bar -> do
            t <- eval ch secondTerm
            case ch of
                Just x -> eval ch (App t (Dot x))
                Nothing -> eval ch (App t V)

        Compare c -> do
            t <- eval ch secondTerm
            case (ch == Just c) of
                True -> eval ch (App t I)
                False -> eval ch (App t V)

