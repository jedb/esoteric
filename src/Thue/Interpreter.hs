module Thue.Interpreter (
    thue,
    extractInfix,
    nextInRange
    ) where


import System.Random
import Data.Maybe
import Data.List
import Thue.Parser



data ThueVersion = Ver1 | Ver2a
    deriving (Eq)


data Choice = Random StdGen
            | First
            | Last



thue :: ThueProgram -> IO ThueState
thue program =
    let rules = thueRules program
        state = thueInitialState program
        gen = mkStdGen 4 --chosen by fair dice roll, guaranteed to be random

    in interpret Ver1 rules (Random gen) state



interpret :: ThueVersion -> [ThueRule] -> Choice -> ThueState -> IO ThueState
interpret ver rules gen state =
    let possibleRules = rules `applicableTo` state
        (ruleToApply, gen') = choose possibleRules gen

    in if (possibleRules == [])
        then return state
        else applyRule ver ruleToApply state >>= interpret ver rules gen'



choose :: [ThueRule] -> Choice -> (ThueRule, Choice)
choose ruleList gen =
    case gen of
        First -> (head ruleList, First)

        Last -> (last ruleList, Last)

        Random g -> let (num, g') = nextInRange 0 (length ruleList - 1) g
                    in (ruleList !! num, Random g')



applyRule :: ThueVersion -> ThueRule -> ThueState -> IO ThueState
applyRule ver rule state =
    let (before, after) = fromJust (extractInfix (original rule) state)

        inputProc = if (ver == Ver1) then return . tStr else return . tLitStr

    in case (replacement rule) of
        x | x == (tStr ":::") ->
            getLine >>= inputProc >>= return . (before ++) . (++ after)

        x:xs | x == (tCh '~') ->
            putStr (fromThueState xs) >> return (before ++ after)

        x -> return (before ++ x ++ after)



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

