module Thue.Interpreter (
    thue,
    extractInfix,
    nextInRange
    ) where


import System.Random
import Data.Maybe
import Data.List
import Thue.Parser




thue :: ThueProgram -> IO ThueState
thue program =
    let rules = thueRules program
        state = thueInitialState program
        gen = mkStdGen 4 --chosen by fair dice roll, guaranteed to be random

    in interpret state rules gen



interpret :: ThueState -> [ThueRule] -> StdGen -> IO ThueState
interpret state rules gen = do
    let possibleRules = rules `applicableTo` state
        ruleToApply = possibleRules !! num

        (num, gen') = nextInRange 0 (length possibleRules - 1) gen

        (before, after) = fromJust (extractInfix (original ruleToApply) state)

    state' <- case (replacement ruleToApply) of
                ":::" -> getLine >>= (\x -> return (before ++ x ++ after))

                '~':xs -> putStr xs >> return (before ++ after)

                x -> return (before ++ x ++ after)

    if (possibleRules == []) then return state else interpret state' rules gen'



extractInfix :: Eq a => [a] -> [a] -> Maybe ([a], [a])
extractInfix subList list =
    let f = (\pre cur -> case (stripPrefix subList cur) of
                            Nothing -> f ((head cur):pre) (tail cur)
                            Just x -> (reverse pre, x))

    in if (subList `isInfixOf` list) then Just (f [] list) else Nothing



nextInRange :: Int -> Int -> StdGen -> (Int, StdGen)
nextInRange low high random =
    let (genLow, genHigh) = genRange random
        (rawNext, random') = next random

        irawNext = fromIntegral rawNext
        igenLow = fromIntegral genLow
        igenHigh = fromIntegral genHigh
        ilow = fromIntegral low
        ihigh = fromIntegral high

        n' = ((irawNext - igenLow) / (igenHigh - igenLow)) * (ihigh - ilow) + ilow

    in (round n', random')



applicableTo :: [ThueRule] -> ThueState -> [ThueRule]
applicableTo ruleList state =
    filter (\r -> (original r) `isInfixOf` state) ruleList

