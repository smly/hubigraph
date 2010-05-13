module Hubigraph.Style (
   changeVStyle, newVStyle, newVStyleWithID, setVStyleAttr,
   changeEStyle, newEStyle, newEStyleWithID, setEStyleAttr,
) where

import Control.Monad.Reader (asks, liftIO)
import Network.XmlRpc.Client (remote)
import Hubigraph.Base

toBool :: IO Int -> IO Bool
toBool x = do
  x' <- x
  return $ if x' == 0 then True else False

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
changeVStyle :: StyleID -> VertexID -> Hubigraph Bool
changeVStyle sid vid =
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
setVStyleAttr :: VAttr -> StyleID -> Hubigraph Bool
setVStyleAttr va sid =
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
changeEStyle :: StyleID -> EdgeID -> Hubigraph Bool
changeEStyle sid eid =
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
setEStyleAttr :: EAttr -> StyleID -> Hubigraph Bool
setEStyleAttr ea sid =
    do serv <- asks server
       liftIO . toBool $ remote serv "ubigraph.set_edge_style_attribute" sid k v
           where (k, v) = toPair ea
