module Unlambda.Interpreter (
    unlambda
    ) where


import Control.Exception( Exception(..), Handler(..), throw, catches )
import Data.Typeable
import Unlambda.Parser



data UnlambdaException = UnlambdaException { endTerm :: UnlambdaTerm }
    deriving (Show, Typeable)

instance Control.Exception.Exception UnlambdaException




unlambda :: UnlambdaTerm -> IO UnlambdaTerm
unlambda term = 
    catches (eval term)
            [ Handler ((\e -> return (endTerm e)) :: UnlambdaException -> IO UnlambdaTerm) ]



eval :: UnlambdaTerm -> IO UnlambdaTerm
eval term =
    case term of
        App f x -> do
            t <- eval f
            apply t x
        _ -> return term



apply :: UnlambdaTerm -> UnlambdaTerm -> IO UnlambdaTerm
apply firstTerm secondTerm =
    case firstTerm of
        K -> eval secondTerm >>= return . Kpartial

        Kpartial x -> eval secondTerm >> return x

        S -> eval secondTerm >>= return . Spartial

        Spartial x -> eval secondTerm >>= return . (Sapp x)

        Sapp x y -> do
            z <- eval secondTerm
            result <- eval (App (App x z) (App y z))
            return result

        I -> eval secondTerm

        V -> eval secondTerm >> return V

        C -> return I --placeholder

        D -> return (Promise secondTerm)

        Promise x -> do
            y <- eval secondTerm
            result <- eval (App x y)
            return result

        Dot c -> putChar c >> eval secondTerm

        R -> putChar '\n' >> eval secondTerm

        E -> do
            t <- eval secondTerm
            throw (UnlambdaException t)

        Reed -> return I --placeholder

        Bar -> return I --placeholder

        Compare c -> return I --placeholder

