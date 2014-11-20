module Grasp.Node (
	GNode,
	GNodeType,

	singleton,
	uSingleton,
	fromStringList,
	lab,
	inst,
	idNo
	) where



import Data.Graph.Inductive.Graph( LNode )
import Data.Map( Map )
import qualified Data.Map as Map



type GNode = LNode (Maybe String, String)
type GNodeType = (Maybe String, String)



singleton :: Int -> String -> String -> GNode
singleton i m s = (i,(Just m,s))

uSingleton :: Int -> String -> GNode
uSingleton i s = (i,(Nothing,s))

fromStringList :: Map String Int -> [(String,String)] -> [GNode]
fromStringList m ns =
    let change x = case (Map.lookup x m) of
    	    Just a -> a
    	    Nothing -> error "Grasp.Node.fromStringList: no value for key " ++ x
    in map (\(x,y) -> (change x, (Just x, y))) ns

lab :: GNode -> Maybe String
lab (_,(x,_)) = x

inst :: GNode -> String
inst (_,(_,x)) = x

idNo :: GNode -> Int
idNo (x,(_,_)) = x

