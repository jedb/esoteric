module Unlambda.Test (
    parserTests,
    interpreterTests,
    tests,
    ioTests
    ) where


import Test.HUnit
import Text.Parsec.Error
import Control.Monad
import System.IO.Silently
import Unlambda.Types
import Unlambda.Parser
import Unlambda.Interpreter


instance Eq Text.Parsec.Error.ParseError




parser0 = (Right S) ~=? (parseUnlambda "s")

parser1 = (Right K) ~=? (parseUnlambda "k")

parser2 = (Right I) ~=? (parseUnlambda "i")

parser3 = (Right V) ~=? (parseUnlambda "v")

parser4 = (Right R) ~=? (parseUnlambda "r")

parser5 = (Right D) ~=? (parseUnlambda "d")

parser6 = (Right C) ~=? (parseUnlambda "c")

parser7 = (Right E) ~=? (parseUnlambda "e")

parser8 = (Right (App S K)) ~=? (parseUnlambda "`sk")

parser9 = (Right (Dot 'c')) ~=? (parseUnlambda ".c")

parser10 = (Right (Compare '?')) ~=? (parseUnlambda "??")

parser11 = (Right Bar) ~=? (parseUnlambda "|")

parser12 = (Right Reed) ~=? (parseUnlambda "@")



interpretString :: String -> IO (Maybe (String,UnlambdaTerm))
interpretString input =
	let t = parseUnlambda input
	in case t of
		Left _ -> return Nothing
		Right term -> do
			c <- capture (unlambda term)
			return (Just c)



interpreter0 = (liftM2 (~=?))
                    (return (Just ("\n",R)) )
                    (interpretString "``cir")

interpreter1 = (liftM2 (~=?))
                    (return (Just ("",I)) )
                    (interpretString "`c``s`kr``si`ki")




parserTests :: Test
parserTests = TestList [parser0, parser1, parser2, parser3, parser4, parser5, parser6, parser7, parser8
                       ,parser9, parser10, parser11, parser12]



interpreterTests :: IO Test
interpreterTests = do
	t0 <- interpreter0
	t1 <- interpreter1
	return (TestList [t0,t1])



tests :: Test
tests = parserTests



ioTests :: IO Test
ioTests = interpreterTests

