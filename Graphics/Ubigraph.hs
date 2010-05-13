module Hubigraph (
   module Hubigraph.Base,
   module Hubigraph.Style,
   --module Hubigraph.Old,
   clear,
   newVertex, removeVertex,
   newEdge, removeEdge,
   newVertexWithID, newEdgeWithID,
   setVAttr, setEAttr,
) where

import Control.Monad.Reader (ReaderT(..), runReaderT, asks, liftIO, lift)
import Network.XmlRpc.Client (remote)
import Data.Char (toLower)
import Hubigraph.Base
import Hubigraph.Style

toBool :: IO Int -> IO Bool
toBool x = do
  x' <- x
  return $ if x' == 0 then True else False

{-
/* Delete all vertices and edges */
void        ubigraph_clear();
-}

clear :: Hubigraph Bool
clear =
    do serv <- asks server
       liftIO . toBool $ remote serv "ubigraph.clear"

{-
/* Basic API methods */
vertex_id_t ubigraph_new_vertex();
edge_id_t   ubigraph_new_edge(vertex_id_t x, vertex_id_t y);
result_t    ubigraph_remove_vertex(vertex_id_t x);
result_t    ubigraph_remove_edge(edge_id_t e);
-}

newVertex :: Hubigraph VertexID
newVertex =
    do serv <- asks server
       liftIO $ remote serv "ubigraph.new_vertex"

removeVertex :: VertexID -> Hubigraph Bool
removeVertex vid =
    do serv <- asks server
       liftIO . toBool $ remote serv "ubigraph.remove_vertex" vid

newEdge :: Edge -> Hubigraph EdgeID
newEdge (src,dst) =
    do serv <- asks server
       liftIO $ remote serv "ubigraph.new_edge" src dst

removeEdge :: EdgeID -> Hubigraph Bool
removeEdge eid =
    do serv <- asks server
       liftIO . toBool $ remote serv "ubigraph.remove_edge" eid

{-
/* Vertex/edge creation when user wants to use their own id's */
result_t    ubigraph_new_vertex_w_id(vertex_id_t x);
result_t    ubigraph_new_edge_w_id(edge_id_t e, vertex_id_t x, vertex_id_t y);
-}

newVertexWithID :: VertexID -> Hubigraph Bool
newVertexWithID node =
    do serv <- asks server
       liftIO . toBool $ remote serv "ubigraph.new_vertex_w_id" node

newEdgeWithID :: EdgeID -> Edge -> Hubigraph Bool
newEdgeWithID eid (src,dst) =
    do serv <- asks server
       liftIO . toBool $ remote serv "ubigraph.new_edge_w_id" eid src dst


-- ################# ATTRIBUTE

{-
/* Set a vertex attribute */
result_t    ubigraph_set_vertex_attribute(vertex_id_t x,
              const char* attribute, const char* value);

/* Set an edge attribute */
result_t    ubigraph_set_edge_attribute(edge_id_t x,
              const char* attribute, const char* value);
-}

setVAttr :: VAttr -> VertexID -> Hubigraph Bool
setVAttr va vid =
    do serv <- asks server
       liftIO . toBool $ remote serv "ubigraph.set_vertex_attribute" vid k v
           where (k, v) = toPair va

setEAttr :: EAttr -> EdgeID -> Hubigraph Bool
setEAttr ea eid =
    do serv <- asks server
       liftIO . toBool $ remote serv "ubigraph.set_edge_attribute" eid k v
           where (k, v) = toPair ea
