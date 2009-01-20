module Hubigraph (
   module Hubigraph.Base,
   initHubigraph, runHubigraph,
   clear, newVertexWithID, newVertex, removeVertex,
   newEdgeWithID, newEdge,
   vertexShape, vertexShapedetail, vertexSize,
   vertexColor, vertexLabel, vertexFontcolor,
   vertexFontfamily, vertexFontsize, vertexVisible,
   edgeColor, edgeLabel, edgeFontcolor,
   edgeFontfamily, edgeFontsize, edgeSpline,
   edgeStrength, edgeOriented, edgeStroke,
   edgeArrow, edgeArrowPosition,
   edgeArrowRadius, edgeArrowLength, edgeArrowReverse,
   edgeShowstrain, edgeVisible, edgeWith,  
  ) where

import Control.Monad.Reader (ReaderT(..), runReaderT, asks, liftIO, lift)
import Network.XmlRpc.Client (remote)
import Data.Char (toLower)

import Hubigraph.Base

runHubigraph = runReaderT

initHubigraph :: (Monad m) => String -> m Ubigraph
initHubigraph serv = return ( Ubigraph { server = serv } )

clear :: Hubigraph Int
clear = 
    do serv <- asks server
       liftIO $ remote serv "ubigraph.clear"

newVertexWithID :: ID -> Hubigraph Int
newVertexWithID node =
    do serv <- asks server
       liftIO $ remote serv "ubigraph.new_vertex_w_id" node

newVertex :: Hubigraph Int
newVertex =
    do serv <- asks server
       liftIO $ remote serv "ubigraph.new_vertex"

removeVertex :: Hubigraph Int
removeVertex =
    do serv <- asks server
       liftIO $ remote serv "ubigraph.remove_vertex"

newEdgeWithID :: ID -> Edge -> Hubigraph Int
newEdgeWithID eid (src,dst) =
    do serv <- asks server
       liftIO $ remote serv "ubigraph.new_edge_w_id" eid src dst

newEdge :: Edge -> Hubigraph Int
newEdge (src,dst) =
    do serv <- asks server
       liftIO $ remote serv "ubigraph.new_edge" src dst

setVertexAttribute :: ID -> String -> String -> Hubigraph Int
setVertexAttribute id op val =
    do serv <- asks server
       liftIO $ remote serv "ubigraph.set_vertex_attribute" id op val

vertexShape :: ID -> Shapes -> Hubigraph Int
vertexShape node shape = setVertexAttribute node "shape" $ map toLower (show shape)

vertexShapedetail :: ID -> Int -> Hubigraph Int
vertexShapedetail node val = setVertexAttribute node "shapedetail" $ show val

vertexColor :: ID -> String -> Hubigraph Int
vertexColor node color = setVertexAttribute node "color" color

vertexSize :: ID -> Float -> Hubigraph Int
vertexSize node sz = setVertexAttribute node "size" $ show sz

vertexLabel :: ID -> String -> Hubigraph Int
vertexLabel node str = setVertexAttribute node "label" str

vertexFontcolor :: ID -> String -> Hubigraph Int
vertexFontcolor node color = setVertexAttribute node "fontcolor" color

vertexFontfamily :: ID -> String -> Hubigraph Int
vertexFontfamily node family = setVertexAttribute node "fontfamily" family

vertexFontsize :: ID -> Int -> Hubigraph Int
vertexFontsize node sz = setVertexAttribute node "fontsize" $ show sz

vertexVisible :: ID -> Bool -> Hubigraph Int
vertexVisible node bl = setVertexAttribute node "visible" $ map toLower (show bl)

setEdgeAttribute :: ID -> String -> String -> Hubigraph Int
setEdgeAttribute eid name val =
    do serv <- asks server
       liftIO $ remote serv "ubigraph.set_edge_attribute" eid name val

edgeArrow :: ID -> Bool -> Hubigraph Int
edgeArrow eid bl = setEdgeAttribute eid "arrow" $ map toLower (show bl)

edgeArrowPosition :: ID -> Float -> Hubigraph Int
edgeArrowPosition eid pos = setEdgeAttribute eid "arrow_position" $ show pos

edgeArrowRadius :: ID -> Float -> Hubigraph Int
edgeArrowRadius eid rad = setEdgeAttribute eid "arrow_radius" $ show rad

edgeArrowLength :: ID -> Float -> Hubigraph Int
edgeArrowLength eid len = setEdgeAttribute eid "arrow_length" $ show len

edgeArrowReverse :: ID -> Bool -> Hubigraph Int
edgeArrowReverse eid bl = setEdgeAttribute eid "arrow_reverse" $ map toLower (show bl)

edgeColor :: ID -> String -> Hubigraph Int
edgeColor eid color = setEdgeAttribute eid "color" color

edgeLabel :: ID -> String -> Hubigraph Int
edgeLabel eid str = setEdgeAttribute eid "label" str

edgeFontcolor :: ID -> String -> Hubigraph Int
edgeFontcolor eid color = setEdgeAttribute eid "fontcolor" color

edgeFontfamily :: ID -> String -> Hubigraph Int
edgeFontfamily eid family = setEdgeAttribute eid "fontfamily" family

edgeFontsize :: ID -> Int -> Hubigraph Int
edgeFontsize eid sz = setEdgeAttribute eid "fontsize" $ show sz

edgeOriented :: ID -> Bool -> Hubigraph Int
edgeOriented eid bl = setEdgeAttribute eid "oriented" $ map toLower (show bl)

edgeSpline :: ID -> Bool -> Hubigraph Int
edgeSpline eid bl = setEdgeAttribute eid "spline" $ map toLower (show bl)

edgeShowstrain :: ID -> Bool -> Hubigraph Int
edgeShowstrain eid bl = setEdgeAttribute eid "showstrain" $ map toLower (show bl)

edgeStroke :: ID -> Stroke -> Hubigraph Int
edgeStroke eid stroke = setEdgeAttribute eid "stroke" $ map toLower (show stroke)

edgeStrength :: ID -> Float -> Hubigraph Int
edgeStrength eid len = setEdgeAttribute eid "strength" $ show len

edgeVisible :: ID -> Bool -> Hubigraph Int
edgeVisible eid bl = setEdgeAttribute eid "visible" $ map toLower (show bl)

edgeWith :: ID -> Float -> Hubigraph Int
edgeWith eid width = setEdgeAttribute eid "width" $ show width
