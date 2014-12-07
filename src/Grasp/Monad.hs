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

    nodesOut, edgesOut,
    reLabel,
    insEdge, insEdges,
    delEdge, delEdges
    ) where




import System.IO( Handle, FilePath, IOMode )
import qualified System.IO as IO

import qualified System.Random as Random

import Control.Monad.Trans.State.Lazy( StateT )
import qualified Control.Monad.Trans.State.Lazy as State

import qualified Control.Monad as Monad

import Control.Monad.IO.Class( liftIO )

import qualified Data.Maybe as Maybe

import Data.List( (\\) )
import qualified Data.List as List

import Data.Map( Map )
import qualified Data.Map as Map

import Grasp.Graph( Node, LNode, LEdge, Gr, (&) )
import qualified Grasp.Graph as Graph

import Grasp.Types.IP( IP )
import qualified Grasp.Types.IP as IP

import Grasp.Types.GNode( GNode )
import qualified Grasp.Types.GNode as GN

import Grasp.Types.GEdge( GEdge )
import qualified Grasp.Types.GEdge as GE

import Grasp.Types.Instruction( Instruction )
import qualified Grasp.Types.Instruction as IN

import Grasp.Types.EdgeLabel( EdgeLabel )
import qualified Grasp.Types.EdgeLabel as EL




type GraspM a = StateT (Gr Instruction EdgeLabel, [IP], Map FilePath Handle) IO a





-- wrapping and unwrapping


construct :: ([GNode],[GEdge]) -> GraspM ()
construct (ns,es) = do
    Monad.when (multiNodes ns) (error "node declared multiple times")
    Monad.when (unconnected ns es) (error "unconnected edge")
    Monad.when (multiNames ns es) (error "node with multiple names")
    Monad.when (numericName ns es) (error "node with a numeric name")
    Monad.when (noMain ns es) (error "could not find grasp:main")

    let graph = Graph.mkGraph (map GN.toLNode ns) (map GE.toLEdge es)
        ips = map IP.singleton (nodesWithName ns es "grasp:main")
        handles = Map.empty

    State.put (graph, ips, handles)



finalise :: GraspM () -> IO ([GNode],[GEdge])
finalise s = do
    (gr, ips, fh) <- State.execStateT s (Graph.empty, [], Map.empty)
    (mapM_ IO.hClose) . Map.elems $ fh
    let (nodes, edges) = (Graph.labNodes gr, Graph.labEdges gr)
    return (map GN.mk nodes, map GE.mk edges)





-- internally used functions


-- fix this later so it doesn't required unconnected edge checking first
nameNodeList :: [GNode] -> [GEdge] -> [(GNode,GNode)]
nameNodeList ns es =
    let nameEdges = filter ((== (EL.mk "name")) . GE.toLabel) es
        findNode n = Maybe.fromJust (List.find ((== n) . GN.toNode) ns)
    in map (\x -> (findNode (GE.toSrc x), findNode (GE.toDest x))) nameEdges



nodesWithName :: [GNode] -> [GEdge] -> String -> [GNode]
nodesWithName ns es name =
    (map fst) . (filter (\x -> (GN.toInst . snd $ x) == (IN.mk name))) $ (nameNodeList ns es)



namedNodes :: [GNode] -> [GEdge] -> [GNode]
namedNodes ns es = map fst (nameNodeList ns es)





-- error checking when setting up


multiNodes :: [GNode] -> Bool
multiNodes ns = (ns == (List.nubBy (\x y -> GN.toNode x == GN.toNode y) ns))



unconnected :: [GNode] -> [GEdge] -> Bool
unconnected ns es =
    let nodeList = map GN.toNode ns
        unconnectedEdges = filter (\x -> GE.toSrc x `notElem` nodeList || GE.toDest x `notElem` nodeList) es
    in unconnectedEdges /= []



multiNames :: [GNode] -> [GEdge] -> Bool
multiNames ns es =
    let named = map fst (nameNodeList ns es)
    in named == (List.nub named)



numericName :: [GNode] -> [GEdge] -> Bool
numericName ns es =
    let names = map snd (nameNodeList ns es)
    in any (\x -> Maybe.isJust (IN.toFloat (GN.toInst x))) names



noMain :: [GNode] -> [GEdge] -> Bool
noMain ns es =
    let names = map snd (nameNodeList ns es)
        mains = filter ((== (IN.mk "grasp:main")) . GN.toInst) names
    in length mains /= 0





-- garbage collection


garbageCollect :: Gr Instruction EdgeLabel -> [IP] -> Gr Instruction EdgeLabel
garbageCollect gr ips =
    let unreachable = (Graph.nodes gr) \\ (reachable gr ips)
    in Graph.delNodes unreachable gr



reachable :: Gr Instruction EdgeLabel -> [IP] -> [Node]
reachable gr ips =
    let named = namedNodes (map GN.mk (Graph.labNodes gr)) (map GE.mk (Graph.labEdges gr))
        ipNodes = concatMap IP.toList ips
        start = (map GN.toNode) . List.nub $ named ++ ipNodes
    in reach gr start []



reach :: Gr Instruction EdgeLabel -> [Node] -> [Node] -> [Node]
reach _ [] f = f
reach gr (x:xs) f =
    let f' = List.nub (x:f)
        x' = List.nub (xs ++ (Graph.suc gr x))
        gr' = Graph.delNode x gr
    in reach gr' x' f'





-- I/O


getWriteHandle :: FilePath -> GraspM Handle
getWriteHandle path = do
    (gr, ips, fh) <- State.get

    case (Map.lookup path fh) of
        Nothing -> do
            h <- liftIO (IO.openFile path IO.AppendMode)
            State.put (gr, ips, (Map.insert path h fh))
            return h

        Just x -> do
            w <- liftIO (IO.hIsWritable x)
            if (not w) then do
                liftIO (IO.hClose x)
                h <- liftIO (IO.openFile path IO.AppendMode)
                State.put (gr, ips, (Map.insert path h fh))
                return h
            else return x



getReadHandle :: FilePath -> GraspM Handle
getReadHandle path = do
    (gr, ips, fh) <- State.get

    case (Map.lookup path fh) of
        Nothing -> do
            h <- liftIO (IO.openFile path IO.ReadMode)
            State.put (gr, ips, (Map.insert path h fh))
            return h

        Just x -> do
            r <- liftIO (IO.hIsReadable x)
            if (not r) then do
                liftIO (IO.hClose x)
                h <- liftIO (IO.openFile path IO.ReadMode)
                State.put (gr, ips, (Map.insert path h fh))
                return h
            else return x





-- manipulating the instruction pointers


updateIP :: GraspM ()
updateIP = do
    (gr, ips, fh) <- State.get
    curNode <- peekIP
    Monad.when (Maybe.isJust curNode) (do
        nexts <- nodesOut (EL.mk "next") (Maybe.fromJust curNode)
        r <- liftIO (Random.getStdRandom (Random.randomR (0, length nexts - 1)))

        let updated = if (length nexts == 0) then IP.empty else IP.shift (nexts !! r) (head ips)
            ips' = updated:(tail ips)

        State.put (gr, ips', fh) )



pushIP :: GNode -> GraspM ()
pushIP n = do
    (gr, ips, fh) <- State.get
    let ips' = if (length ips == 0) then [] else (IP.push n (head ips)):(tail ips)
    State.put (gr, ips', fh)



popIP :: GraspM ()
popIP = do
    (gr, ips, fh) <- State.get
    let ips' = if (length ips == 0) then [] else (IP.pop (head ips)):(tail ips)
    State.put (gr, ips', fh)



peekIP :: GraspM (Maybe GNode)
peekIP = do
    (gr, ips, fh) <- State.get
    if (length ips == 0) then return Nothing else return (IP.peek (head ips))



nextIP :: GraspM ()
nextIP = do
    (gr, ips, fh) <- State.get
    let ips' = if (length ips == 0)
                then []
                else if (IP.isEmpty (head ips))
                        then tail ips
                        else (tail ips) ++ [head ips]
        gr' = garbageCollect gr ips'
    State.put (gr', ips', fh)





-- accessing and manipulating the graph


nodesOut :: EdgeLabel -> GNode -> GraspM [GNode]
nodesOut s n = do
    (gr, ips, fh) <- State.get
    curNode <- peekIP

    let nout = Graph.lsuc gr (GN.toNode (Maybe.fromJust curNode))
        filtered = filter ((== s) . snd) nout
        result = map (\(x,y) -> GN.mk (x, Maybe.fromJust (Graph.lab gr x))) filtered

    if (Maybe.isNothing curNode) then return [] else return result



edgesOut :: GNode -> GraspM [GEdge]
edgesOut n = do
    (gr, ips, fh) <- State.get
    curNode <- peekIP

    let eout = Graph.out gr (GN.toNode (Maybe.fromJust curNode))
        result = map GE.mk eout

    if (Maybe.isNothing curNode) then return [] else return result



reLabel :: Instruction -> GNode -> GraspM ()
reLabel i n = do
    (gr, ips, fh) <- State.get

    let (mc, d) = Graph.match (GN.toNode n) gr
        c = Maybe.fromJust mc
        c' = (\(w,x,y,z) -> (w,x,i,z)) $ c

    Monad.when (Maybe.isJust mc) (State.put ((c' & d) ,ips, fh))



insEdge :: GEdge -> GraspM ()
insEdge e = do
    (gr, ips, fh) <- State.get
    let gr' = Graph.insEdge (GE.toLEdge e) gr
    State.put (gr', ips, fh)



insEdges :: [GEdge] -> GraspM ()
insEdges es = do
    (gr, ips, fh) <- State.get
    let gr' = Graph.insEdges (map GE.toLEdge es) gr
    State.put (gr', ips, fh)



delEdge :: GEdge -> GraspM ()
delEdge e = do
    (gr, ips, fh) <- State.get
    let gr' = Graph.delLEdge (GE.toLEdge e) gr
    State.put (gr', ips, fh)



delEdges :: [GEdge] -> GraspM ()
delEdges es = do
    (gr, ips, fh) <- State.get
    let gr' = List.foldl' (flip Graph.delLEdge) gr (map GE.toLEdge es)
    State.put (gr', ips, fh)

