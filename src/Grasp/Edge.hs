module Grasp.Edge (
	GEdge,
	GEdgeType,

	singleton,
	fromStringList,
	src,
	dest,
	lab
	) where



import Data.Graph.Inductive.Graph( LEdge )
import Data.Map( Map )
import qualified Data.Map as Map



type GEdge = LEdge String
type GEdgeType = String



singleton :: Int -> Int -> String -> GEdge
singleton f t s = (f,t,s)

fromStringList :: Map String Int -> [(String,String,String)] -> [GEdge]
fromStringList m es =
	let change x = case (Map.lookup x m) of
		    Just a -> a
		    Nothing -> error "Grasp.Edge.fromStringList: no value for key " ++ x
    in map (\(x,y,z) -> (change x, change y, z)) es

src :: GEdge -> Int
src (x,_,_) = x

dest :: GEdge -> Int
dest (_,x,_) = x

lab :: GEdge -> String
lab (_,_,x) = x

