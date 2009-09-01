module Hubigraph (
   module Hubigraph.Base,
   --module Hubigraph.Old,
   initHubigraph, runHubigraph,
   clear,
   newVertexWithID, newVertex, removeVertex,
   newEdgeWithID, newEdge,
  ) where

import Control.Monad.Reader (ReaderT(..), runReaderT, asks, liftIO, lift)
import Network.XmlRpc.Client (remote)
import Data.Char (toLower)

import Hubigraph.Base

{-
typedef int32_t result_t;
typedef int32_t vertex_id_t;
typedef int32_t edge_id_t;
typedef int32_t style_id_t;

/* Basic API methods */
vertex_id_t ubigraph_new_vertex();
edge_id_t   ubigraph_new_edge(vertex_id_t x, vertex_id_t y);
result_t    ubigraph_remove_vertex(vertex_id_t x);
result_t    ubigraph_remove_edge(edge_id_t e);

/* Vertex/edge creation when user wants to use their own id's */
result_t    ubigraph_new_vertex_w_id(vertex_id_t x);
result_t    ubigraph_new_edge_w_id(edge_id_t e, vertex_id_t x, vertex_id_t y);

/* Delete all vertices and edges */
void        ubigraph_clear();

/* Set a vertex attribute */
result_t    ubigraph_set_vertex_attribute(vertex_id_t x,
              const char* attribute, const char* value);

/* Vertex styles */
result_t    ubigraph_change_vertex_style(vertex_id_t x, style_id_t s);
style_id_t  ubigraph_new_vertex_style(style_id_t parent_style);
result_t    ubigraph_new_vertex_style_w_id(style_id_t s, 
              style_id_t parent_style);
result_t    ubigraph_set_vertex_style_attribute(style_id_t s,
              const char* attribute, const char* value);

/* Set an edge attribute */
result_t    ubigraph_set_edge_attribute(edge_id_t x,
              const char* attribute, const char* value);

/* Edge styles */
result_t    ubigraph_change_edge_style(edge_id_t x, style_id_t s);
style_id_t  ubigraph_new_edge_style(style_id_t parent_style);
result_t    ubigraph_new_edge_style_w_id(style_id_t s,
              style_id_t parent_style);
result_t    ubigraph_set_edge_style_attribute(style_id_t s,
              const char* attribute, const char* value);
-}

-- for debug
r x = initHubigraph "http://localhost:20738/RPC2" >>= runHubigraph x

runHubigraph = runReaderT

initHubigraph :: (Monad m) => String -> m Ubigraph
initHubigraph serv = return ( Ubigraph { server = serv } )

toBool :: IO Int -> IO Bool
toBool x = do
  x' <- x
  return $ if x' == 0 then True else False

clear :: Hubigraph Bool
clear =
    do serv <- asks server
       liftIO . toBool $ remote serv "ubigraph.clear"

newVertexWithID :: VertexID -> Hubigraph Bool
newVertexWithID node =
    do serv <- asks server
       liftIO . toBool $ remote serv "ubigraph.new_vertex_w_id" node

newVertex :: Hubigraph VertexID
newVertex =
    do serv <- asks server
       liftIO $ remote serv "ubigraph.new_vertex"

removeVertex :: VertexID -> Hubigraph Bool
removeVertex vid =
    do serv <- asks server
       liftIO . toBool $ remote serv "ubigraph.remove_vertex" vid

newEdgeWithID :: EdgeID -> Edge -> Hubigraph Bool
newEdgeWithID eid (src,dst) =
    do serv <- asks server
       liftIO . toBool $ remote serv "ubigraph.new_edge_w_id" eid src dst

newEdge :: Edge -> Hubigraph EdgeID
newEdge (src,dst) =
    do serv <- asks server
       liftIO $ remote serv "ubigraph.new_edge" src dst

removeEdge :: EdgeID -> Hubigraph Bool
removeEdge eid =
    do serv <- asks server
       liftIO . toBool $ remote serv "ubigraph.remove_edge" eid

-- #################
setEdgeAttribute :: EdgeID -> EAttr -> Hubigraph Bool
setEdgeAttribute eid ea =
    do serv <- asks server
       liftIO . toBool $ remote serv "ubigraph.set_edge_attribute"

changeVertexStyle :: VertexID -> StyleID -> Hubigraph Bool
changeVertexStyle vid sid =
    do serv <- asks server
       liftIO . toBool $ remote serv "ubigraph.change_vertex_style" vid sid

setVertexStyleAttribute :: StyleID -> VAttr -> Hubigraph Bool
setVertexStyleAttribute sid va =
    do serv <- asks server
       liftIO . toBool $ remote serv "ubigraph.set_vertex_style_attribute" sid k v
           where (k, v) = toPair va

newVertexStyle :: StyleID -> Hubigraph StyleID
newVertexStyle sid =
    do serv <- asks server
       liftIO $ remote serv "ubigraph.new_vertex_style" sid
