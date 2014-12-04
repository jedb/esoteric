module Grasp.Monad (
    GraspM,

    construct,
    finalise,

    getReadHandle,
    getWriteHandle,

    updateIP,
    pushIP,
    popIP,
    peekIP,
    nextIP,

    nodesOut
    ) where



import System.IO( Handle, FilePath, IOMode )
import qualified System.IO as IO

import qualified System.Random as Random

import Control.Monad.Trans.State.Lazy( StateT )
import qualified Control.Monad.Trans.State.Lazy as State

import qualified Control.Monad as Monad

import Control.Monad.IO.Class( liftIO )

import Text.Read( readMaybe )

import qualified Data.Maybe as Maybe

import qualified Data.List as List

import Data.Map( Map )
import qualified Data.Map as Map

import Grasp.Graph( Node, LNode, LEdge, Gr )
import qualified Grasp.Graph as Graph

import Grasp.IP( IP )
import qualified Grasp.IP as IP

import Grasp.Types( Instruction(..), EdgeLabel(..), GNode(..), GEdge(..) )
import qualified Grasp.Types as Types




type GraspM a = StateT GraspProgram IO a



data GraspProgram = GraspProgram { programGraph :: Gr Instruction EdgeLabel
                                 , instPtrs :: [IP]
                                 , fileHandles :: Map FilePath Handle }
    deriving (Show, Eq)




construct :: ([GNode],[GEdge]) -> GraspM ()
construct (n,e) = do
    Monad.when (multiNodes n) (error "node declared multiple times")
    Monad.when (unconnected n e) (error "unconnected edge")
    Monad.when (multiNames n e) (error "node with multiple names")
    Monad.when (numericName n e) (error "node with a numeric name")
    Monad.when (noMain n e) (error "could not find grasp:main")

    let graph = Graph.mkGraph (map toLNode n) (map toLEdge e)
        ips = map IP.singleton (nodesWithName n e "grasp:main")
        handles = Map.empty

    State.put (GraspProgram graph ips handles)



finalise :: GraspM () -> IO ([GNode],[GEdge])
finalise s = do
    p <- State.execStateT s (GraspProgram Graph.empty [] Map.empty)
    (mapM_ IO.hClose) . Map.elems . fileHandles $ p
    let gr = programGraph p
        (nodes, edges) = (Graph.labNodes gr, Graph.labEdges gr)
    return (map GNode nodes, map GEdge edges)



toLNode :: GNode -> LNode Instruction
toLNode n = (Types.gnode n, Types.gninst n)



toLEdge :: GEdge -> LEdge EdgeLabel
toLEdge e = (Types.gefrom e, Types.geto e, Types.gelabel e)



multiNodes :: [GNode] -> Bool
multiNodes ns = (ns == (List.nubBy (\x y -> Types.gnode x == Types.gnode y) ns))



unconnected :: [GNode] -> [GEdge] -> Bool
unconnected ns es =
    let nodeList = map Types.gnode ns
        unconnectedEdges = filter (\x -> Types.gefrom x `notElem` nodeList || Types.geto x `notElem` nodeList) es
    in unconnectedEdges /= []



multiNames :: [GNode] -> [GEdge] -> Bool
multiNames ns es =
    let named = map fst (nameNodeList ns es)
    in named == (List.nub named)



numericName :: [GNode] -> [GEdge] -> Bool
numericName ns es =
    let names = map snd (nameNodeList ns es)
        test (Instruction x) = readMaybe x :: Maybe Float
    in any (\x -> Maybe.isJust (test (Types.gninst x))) names



noMain :: [GNode] -> [GEdge] -> Bool
noMain ns es =
    let names = map snd (nameNodeList ns es)
        mains = filter ((== (Instruction "grasp:main")) . Types.gninst) names
    in length mains /= 0



getWriteHandle :: FilePath -> GraspM Handle
getWriteHandle path = do
    program <- State.get
    let (gr, ptrs, handles) = ((programGraph program), (instPtrs program), (fileHandles program))

    case (Map.lookup path handles) of
        Nothing -> do
            h <- liftIO (IO.openFile path IO.AppendMode)
            State.put (GraspProgram gr ptrs (Map.insert path h handles))
            return h

        Just x -> do
            w <- liftIO (IO.hIsWritable x)
            if (not w) then do
                liftIO (IO.hClose x)
                h <- liftIO (IO.openFile path IO.AppendMode)
                State.put (GraspProgram gr ptrs (Map.insert path h handles))
                return h
            else return x



getReadHandle :: FilePath -> GraspM Handle
getReadHandle path = do
    program <- State.get
    let (gr, ptrs, handles) = ((programGraph program), (instPtrs program), (fileHandles program))

    case (Map.lookup path (fileHandles program)) of
        Nothing -> do
            h <- liftIO (IO.openFile path IO.ReadMode)
            State.put (GraspProgram gr ptrs (Map.insert path h handles))
            return h

        Just x -> do
            r <- liftIO (IO.hIsReadable x)
            if (not r) then do
                liftIO (IO.hClose x)
                h <- liftIO (IO.openFile path IO.ReadMode)
                State.put (GraspProgram gr ptrs (Map.insert path h handles))
                return h
            else return x



-- fix this later so it doesn't required unconnected edge checking first
nameNodeList :: [GNode] -> [GEdge] -> [(GNode,GNode)]
nameNodeList ns es =
    let nameEdges = filter ((== (EdgeLabel "name")) . Types.gelabel) es
        findNode n = Maybe.fromJust (List.find ((== n) . Types.gnode) ns)
    in map (\x -> (findNode (Types.gefrom x), findNode (Types.geto x))) nameEdges



nodesWithName :: [GNode] -> [GEdge] -> String -> [GNode]
nodesWithName ns es name =
    (map fst) . (filter (\x -> (Types.gninst . snd $ x) == (Instruction name))) $ (nameNodeList ns es)



updateIP :: GraspM ()
updateIP = do
    program <- State.get
    curNode <- peekIP
    Monad.when (Maybe.isJust curNode) (do
        nexts <- nodesOut (EdgeLabel "next") (Maybe.fromJust curNode)
        r <- liftIO (Random.getStdRandom (Random.randomR (0, length nexts - 1)))

        let ips = instPtrs program
            updated = if (length nexts == 0) then IP.empty else IP.shift (nexts !! r) (head ips)
            ips' = updated:(tail ips)

        State.put (GraspProgram (programGraph program) ips' (fileHandles program)) )



pushIP :: GNode -> GraspM ()
pushIP n = do
    program <- State.get
    let ips = instPtrs program
        ips' = if (length ips == 0) then [] else (IP.push n (head ips)):(tail ips)
    State.put (GraspProgram (programGraph program) ips' (fileHandles program))



popIP :: GraspM ()
popIP = do
    program <- State.get
    let ips = instPtrs program
        ips' = if (length ips == 0) then [] else (IP.pop (head ips)):(tail ips)
    State.put (GraspProgram (programGraph program) ips' (fileHandles program))



peekIP :: GraspM (Maybe GNode)
peekIP = do
    program <- State.get
    let ips = instPtrs program
    if (length ips == 0) then return Nothing else return (IP.peek (head ips))



nextIP :: GraspM ()
nextIP = do
    program <- State.get
    let ips = instPtrs program
        ips' = if (length ips == 0)
                then []
                else if (IP.isEmpty (head ips))
                        then tail ips
                        else (tail ips) ++ [head ips]
    State.put (GraspProgram (programGraph program) ips' (fileHandles program))



nodesOut :: EdgeLabel -> GNode -> GraspM [GNode]
nodesOut s n = do
    program <- State.get
    curNode <- peekIP

    let gr = programGraph program
        nout = Graph.lsuc gr (Types.gnode (Maybe.fromJust curNode))
        filtered = filter ((== s) . snd) nout
        result = map (\(x,y) -> GNode (x, Maybe.fromJust (Graph.lab gr x))) filtered

    if (Maybe.isNothing curNode) then return [] else return result

