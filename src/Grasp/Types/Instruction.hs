module Grasp.Types.Instruction (
	Instruction,

	mk,

	toString,
	toFloat,
	toInt
    ) where




import Text.Read( readMaybe )




newtype Instruction = Instruction String
    deriving (Show, Eq)




mk :: String -> Instruction
mk = Instruction

toString :: Instruction -> String
toString (Instruction i) = i

toFloat :: Instruction -> Maybe Float
toFloat (Instruction i) = readMaybe i

toInt :: Instruction -> Maybe Int
toInt (Instruction i) = do
	f <- (readMaybe :: String -> Maybe Float) i
	let g = round f
	    h = fromIntegral g
	r <- if (f == h) then Just g else Nothing
	return r

