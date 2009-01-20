module Hubigraph.Graph (
   module Hubigraph,
   addGraphSimple, addGraph
  ) where

import Control.Monad.Reader (ReaderT(..), runReaderT, asks, liftIO, lift)
import Network.XmlRpc.Client (remote)
import Data.Graph as G

import Hubigraph.Base
import Hubigraph

addGraphSimple :: Graph -> Hubigraph ()
addGraphSimple g =
    do mapM_ newVertexWithID (vertices g)
       mapM_ newEdge (edges g)

addGraph :: Graph -> Int -> Int -> Hubigraph ()
addGraph g vid eid =
    do mapM_ (\n -> newVertexWithID (fixv vid n)) (vertices g)
       mapM_ (\(a,b) -> newEdgeWithID b (fixe vid a))   (zip (edges g) (iterate (+1) eid))
           where
             fixe vid (src,dst) = (src+vid, dst+vid)
             fixv vid v = v+vid
