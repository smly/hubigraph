module Hubigraph.Old (
   module Hubigraph.Base,
   clear,
   newVertex, removeVertex,
   newEdge, removeEdge,
   newVertexWithID, newEdgeWithID,
   vertexShape, vertexShapedetail, vertexSize,
   vertexColor, vertexLabel, vertexFontcolor,
   vertexFontfamily, vertexFontsize, vertexVisible,
   edgeColor, edgeLabel, edgeFontcolor,
   edgeFontfamily, edgeFontsize, edgeSpline,
   edgeStrength, edgeOriented, edgeStroke,
   edgeArrow, edgeArrowPosition,
   edgeArrowRadius, edgeArrowLength, edgeArrowReverse,
   edgeShowstrain, edgeVisible, edgeWidth,
  ) where

import Control.Monad.Reader (ReaderT(..), runReaderT, asks, liftIO, lift)
import Network.XmlRpc.Client (remote)
import Data.Char (toLower)
import Hubigraph.Base

toBool :: IO Int -> IO Bool
toBool x = do
  x' <- x
  return $ if x' == 0 then True else False

clear :: Hubigraph Bool
clear =
    do serv <- asks server
       liftIO . toBool $ remote serv "ubigraph.clear"

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

newVertexWithID :: VertexID -> Hubigraph Bool
newVertexWithID node =
    do serv <- asks server
       liftIO . toBool $ remote serv "ubigraph.new_vertex_w_id" node

newEdgeWithID :: EdgeID -> Edge -> Hubigraph Bool
newEdgeWithID eid (src,dst) =
    do serv <- asks server
       liftIO . toBool $ remote serv "ubigraph.new_edge_w_id" eid src dst

-- ################# old interface (version ~0.2)
setVertexAttribute :: VertexID -> String -> String -> Hubigraph Bool
setVertexAttribute id op val =
    do serv <- asks server
       liftIO . toBool $ remote serv "ubigraph.set_vertex_attribute" id op val

vertexShape :: VertexID -> Shape -> Hubigraph Bool
vertexShape node shape = setVertexAttribute node "shape" $ map toLower (show shape)

vertexShapedetail :: VertexID -> Int -> Hubigraph Bool
vertexShapedetail node val = setVertexAttribute node "shapedetail" $ show val

vertexColor :: VertexID -> String -> Hubigraph Bool
vertexColor node color = setVertexAttribute node "color" color

vertexSize :: VertexID -> Float -> Hubigraph Bool
vertexSize node sz = setVertexAttribute node "size" $ show sz

vertexLabel :: VertexID -> String -> Hubigraph Bool
vertexLabel node str = setVertexAttribute node "label" str

vertexFontcolor :: VertexID -> String -> Hubigraph Bool
vertexFontcolor node color = setVertexAttribute node "fontcolor" color

vertexFontfamily :: VertexID -> String -> Hubigraph Bool
vertexFontfamily node family = setVertexAttribute node "fontfamily" family

vertexFontsize :: VertexID -> Int -> Hubigraph Bool
vertexFontsize node sz = setVertexAttribute node "fontsize" $ show sz

vertexVisible :: VertexID -> Bool -> Hubigraph Bool
vertexVisible node bl = setVertexAttribute node "visible" $ map toLower (show bl)

-- ################# old interface (version ~0.2)
setEdgeAttribute :: EdgeID -> String -> String -> Hubigraph Bool
setEdgeAttribute eid name val =
    do serv <- asks server
       liftIO . toBool $ remote serv "ubigraph.set_edge_attribute" eid name val

edgeArrow :: EdgeID -> Bool -> Hubigraph Bool
edgeArrow eid bl = setEdgeAttribute eid "arrow" $ map toLower (show bl)

edgeArrowPosition :: EdgeID -> Float -> Hubigraph Bool
edgeArrowPosition eid pos = setEdgeAttribute eid "arrow_position" $ show pos

edgeArrowRadius :: EdgeID -> Float -> Hubigraph Bool
edgeArrowRadius eid rad = setEdgeAttribute eid "arrow_radius" $ show rad

edgeArrowLength :: EdgeID -> Float -> Hubigraph Bool
edgeArrowLength eid len = setEdgeAttribute eid "arrow_length" $ show len

edgeArrowReverse :: EdgeID -> Bool -> Hubigraph Bool
edgeArrowReverse eid bl = setEdgeAttribute eid "arrow_reverse" $ map toLower (show bl)

edgeColor :: EdgeID -> String -> Hubigraph Bool
edgeColor eid color = setEdgeAttribute eid "color" color

edgeLabel :: EdgeID -> String -> Hubigraph Bool
edgeLabel eid str = setEdgeAttribute eid "label" str

edgeFontcolor :: EdgeID -> String -> Hubigraph Bool
edgeFontcolor eid color = setEdgeAttribute eid "fontcolor" color

edgeFontfamily :: EdgeID -> String -> Hubigraph Bool
edgeFontfamily eid family = setEdgeAttribute eid "fontfamily" family

edgeFontsize :: EdgeID -> Int -> Hubigraph Bool
edgeFontsize eid sz = setEdgeAttribute eid "fontsize" $ show sz

edgeOriented :: EdgeID -> Bool -> Hubigraph Bool
edgeOriented eid bl = setEdgeAttribute eid "oriented" $ map toLower (show bl)

edgeSpline :: EdgeID -> Bool -> Hubigraph Bool
edgeSpline eid bl = setEdgeAttribute eid "spline" $ map toLower (show bl)

edgeShowstrain :: EdgeID -> Bool -> Hubigraph Bool
edgeShowstrain eid bl = setEdgeAttribute eid "showstrain" $ map toLower (show bl)

edgeStroke :: EdgeID -> Stroke -> Hubigraph Bool
edgeStroke eid stroke = setEdgeAttribute eid "stroke" $ map toLower (show stroke)

edgeStrength :: EdgeID -> Float -> Hubigraph Bool
edgeStrength eid len = setEdgeAttribute eid "strength" $ show len

edgeVisible :: EdgeID -> Bool -> Hubigraph Bool
edgeVisible eid bl = setEdgeAttribute eid "visible" $ map toLower (show bl)

edgeWidth :: EdgeID -> Float -> Hubigraph Bool
edgeWidth eid width = setEdgeAttribute eid "width" $ show width

