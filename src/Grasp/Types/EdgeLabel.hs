module Grasp.Types.EdgeLabel (
	EdgeLabel,

	mk,

	toString
    ) where




newtype EdgeLabel = EdgeLabel String
    deriving (Show, Eq)




mk :: String -> EdgeLabel
mk = EdgeLabel

toString :: EdgeLabel -> String
toString (EdgeLabel e) = e

