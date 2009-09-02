module Hubigraph (
   module Hubigraph.Base,
   --module Hubigraph.Old,
   initHubigraph, runHubigraph,
   clear,
   newVertex, removeVertex,
   newEdge, removeEdge,
   newVertexWithID, newEdgeWithID,
   setVAttr, setEAttr,
   changeVStyle, newVStyle, newVStyleWithID, setVStyleAttr,
   changeEStyle, newEStyle, newEStyleWithID, setEStyleAttr,
) where

import Control.Monad.Reader (ReaderT(..), runReaderT, asks, liftIO, lift)
import Network.XmlRpc.Client (remote)
import Data.Char (toLower)
import Hubigraph.Base

-- for debug
r x = initHubigraph "http://localhost:20738/RPC2" >>= runHubigraph x

runHubigraph = runReaderT

initHubigraph :: (Monad m) => String -> m Ubigraph
initHubigraph serv = return ( Ubigraph { server = serv } )

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

setVAttr :: VertexID -> VAttr -> Hubigraph Bool
setVAttr vid va =
    do serv <- asks server
       liftIO . toBool $ remote serv "ubigraph.set_vertex_attribute" vid k v
           where (k, v) = toPair va

setEAttr :: EdgeID -> EAttr -> Hubigraph Bool
setEAttr eid ea =
    do serv <- asks server
       liftIO . toBool $ remote serv "ubigraph.set_edge_attribute" eid k v
           where (k, v) = toPair ea

-- ################# STYLE
{-
/* Vertex styles */
result_t    ubigraph_change_vertex_style(vertex_id_t x, style_id_t s);
style_id_t  ubigraph_new_vertex_style(style_id_t parent_style);
result_t    ubigraph_new_vertex_style_w_id(style_id_t s, 
              style_id_t parent_style);
result_t    ubigraph_set_vertex_style_attribute(style_id_t s,
              const char* attribute, const char* value);
-}

-- ubigraph_change_vertex_style
changeVStyle :: VertexID -> StyleID -> Hubigraph Bool
changeVStyle vid sid =
    do serv <- asks server
       liftIO . toBool $ remote serv "ubigraph.change_vertex_style" vid sid

-- ubigraph_new_vertex_style
newVStyle :: StyleID -> Hubigraph StyleID
newVStyle sid =
    do serv <- asks server
       liftIO $ remote serv "ubigraph.new_vertex_style" sid

-- ubigraph_new_vertex_style_w_id
newVStyleWithID :: StyleID -> StyleID -> Hubigraph Bool
newVStyleWithID newid parentid =
    do serv <- asks server
       liftIO $ remote serv "ubigraph.new_vertex_style_w_id" newid parentid

-- ubigraph_set_vertex_style_attribute
setVStyleAttr :: StyleID -> VAttr -> Hubigraph Bool
setVStyleAttr sid va =
    do serv <- asks server
       liftIO . toBool $ remote serv "ubigraph.set_vertex_style_attribute" sid k v
           where (k, v) = toPair va

{-
/* Edge styles */
result_t    ubigraph_change_edge_style(edge_id_t x, style_id_t s);
style_id_t  ubigraph_new_edge_style(style_id_t parent_style);
result_t    ubigraph_new_edge_style_w_id(style_id_t s,
              style_id_t parent_style);
result_t    ubigraph_set_edge_style_attribute(style_id_t s,
              const char* attribute, const char* value);
-}

-- ubigraph_change_edge_style
changeEStyle :: EdgeID -> StyleID -> Hubigraph Bool
changeEStyle eid sid =
    do serv <- asks server
       liftIO . toBool $ remote serv "ubigraph.change_edge_style" eid sid

-- ubigraph_new_edge_style
newEStyle :: StyleID -> Hubigraph StyleID
newEStyle sid =
    do serv <- asks server
       liftIO $ remote serv "ubigraph.new_edge_style" sid

-- ubigraph_new_edge_style_w_id
newEStyleWithID :: StyleID -> StyleID -> Hubigraph Bool
newEStyleWithID newid parentid =
    do serv <- asks server
       liftIO $ remote serv "ubigraph.new_edge_style_w_id" newid parentid

-- ubigraph_set_style_attribute
setEStyleAttr :: StyleID -> EAttr -> Hubigraph Bool
setEStyleAttr sid ea =
    do serv <- asks server
       liftIO . toBool $ remote serv "ubigraph.set_edge_style_attribute" sid k v
           where (k, v) = toPair ea
