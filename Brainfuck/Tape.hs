module Brainfuck.Tape (
    Tape,

    shiftLeft,
    shiftRight,
    currentCell,
    applyToCurrentCell
    ) where


import Control.Monad
import Data.Maybe



type Tape a = ([a], Maybe a, [a])




shiftLeft :: Eq a => Tape a -> Tape a
shiftLeft (x,y,z) = 
	let x' = if (x /= []) then tail x else x
	    y' = if (x /= []) then Just (head x) else Nothing
	    z' = if (isJust y) then (fromJust y):z else z
	in (x', y', z')



shiftRight :: Eq a => Tape a -> Tape a
shiftRight (x,y,z) =
	let x' = if (isJust y) then (fromJust y):x else x
	    y' = if (z /= []) then Just (head z) else Nothing
	    z' = if (z /= []) then tail z else z
	in (x', y', z')



currentCell :: Tape a -> Maybe a
currentCell (_,c,_) = c



applyToCurrentCell :: (a -> a) -> Tape a -> Tape a
applyToCurrentCell f (x,y,z) = (x, (liftM f) y, z)

