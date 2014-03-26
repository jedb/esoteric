module Brainfuck.Interpreter (
    brainfuck
    ) where


import Data.Char
import Data.Maybe
import Brainfuck.Parser
import Brainfuck.Tape




brainfuck :: BrainfuckProgram -> IO (Tape Int)
brainfuck program =
    let dataTape = (repeat 0, Just 0, repeat 0)
        commandTape = shiftRight ([], Nothing, program)
    in process commandTape dataTape



endLoop :: Tape BrainCom -> Tape BrainCom
endLoop tape =
    let f t c = case (currentCell t, c) of
                    (Just CloseLoop, 0) -> shiftRight t
                    (Just OpenLoop, x) -> f (shiftRight t) (x + 1)
                    (Just CloseLoop, x) -> f (shiftRight t) (x - 1)
                    (Just com, x) -> f (shiftRight t) x
    in f (shiftRight tape) 0



doLoop :: Tape BrainCom -> Tape BrainCom
doLoop tape =
    let f t c = case (currentCell t, c) of
                    (Just OpenLoop, 0) -> shiftRight t
                    (Just OpenLoop, x) -> f (shiftLeft t) (x - 1)
                    (Just CloseLoop, x) -> f (shiftLeft t) (x + 1)
                    (Just com, x) -> f (shiftLeft t) x
    in f (shiftLeft tape) 0



process :: Tape BrainCom -> Tape Int -> IO (Tape Int)
process commandTape dataTape =
    case (currentCell commandTape) of
        Nothing -> return dataTape

        Just R -> process (shiftRight commandTape) (shiftRight dataTape)

        Just L -> process (shiftRight commandTape) (shiftLeft dataTape)

        Just Inc -> process (shiftRight commandTape) (applyToCurrentCell ((`mod` 256) . (+1)) dataTape)

        Just Dec -> process (shiftRight commandTape) (applyToCurrentCell ((`mod` 256) . (subtract 1)) dataTape)

        Just Out -> (putChar . chr . fromJust . currentCell $ dataTape) >> process (shiftRight commandTape) dataTape

        Just In -> do { c <- getChar; process (shiftRight commandTape) (applyToCurrentCell (\_ -> ord c) dataTape) }

        Just OpenLoop ->
                if (currentCell dataTape == Just 0)
                then process (endLoop commandTape) dataTape
                else process (shiftRight commandTape) dataTape

        Just CloseLoop ->
                if (currentCell dataTape /= Just 0)
                then process (doLoop commandTape) dataTape
                else process (shiftRight commandTape) dataTape

