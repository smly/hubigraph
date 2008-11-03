module HUBIGraph (
   HUBIGraph, ID, Edge, Shapes(..), Stroke(..),
   initHUBIGraph,
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

import System.IO
import System.Exit (exitWith, ExitCode(ExitSuccess))
import Control.Monad.Reader (ReaderT(..), runReaderT, asks, liftIO, lift)
import Network.XmlRpc.Client
import Char (toLower)

type HUBIGraph = ReaderT UBIGraph IO
data UBIGraph = UBIGraph { server :: String }

type ID = Int
type Edge = (Int, Int)
data Shapes = Cone | Cube | Dodecahedron | Icosahedron
            | Octahedron | Sphere | Tetrahedron
            | Torus deriving (Show)
data Stroke = Solid | Dashed | Dotted
            | None deriving (Show)

initHUBIGraph :: (Monad m) => String -> m UBIGraph
initHUBIGraph serv = return ( UBIGraph { server = serv } )

clear :: HUBIGraph Int
clear = 
    do serv <- asks server
       liftIO $ remote serv "ubigraph.clear"

newVertexWithID :: ID -> HUBIGraph Int
newVertexWithID node =
    do serv <- asks server
       liftIO $ remote serv "ubigraph.new_vertex_w_id" node

newVertex :: HUBIGraph Int
newVertex =
    do serv <- asks server
       liftIO $ remote serv "ubigraph.new_vertex"

removeVertex :: HUBIGraph Int
removeVertex =
    do serv <- asks server
       liftIO $ remote serv "ubigraph.remove_vertex"

newEdgeWithID :: ID -> Edge -> HUBIGraph Int
newEdgeWithID eid (src,dst) =
    do serv <- asks server
       liftIO $ remote serv "ubigraph.new_edge_w_id" eid src dst

newEdge :: Edge -> HUBIGraph Int
newEdge (src,dst) =
    do serv <- asks server
       liftIO $ remote serv "ubigraph.new_edge" src dst

setVertexAttribute :: ID -> String -> String -> HUBIGraph Int
setVertexAttribute id op val =
    do serv <- asks server
       liftIO $ remote serv "ubigraph.set_vertex_attribute" id op val

vertexShape :: ID -> Shapes -> HUBIGraph Int
vertexShape node shape = setVertexAttribute node "shape" $ map toLower (show shape)

vertexShapedetail :: ID -> Int -> HUBIGraph Int
vertexShapedetail node val = setVertexAttribute node "shapedetail" $ show val

vertexColor :: ID -> String -> HUBIGraph Int
vertexColor node color = setVertexAttribute node "color" color

vertexSize :: ID -> Float -> HUBIGraph Int
vertexSize node sz = setVertexAttribute node "size" $ show sz

vertexLabel :: ID -> String -> HUBIGraph Int
vertexLabel node str = setVertexAttribute node "label" str

vertexFontcolor :: ID -> String -> HUBIGraph Int
vertexFontcolor node color = setVertexAttribute node "fontcolor" color

vertexFontfamily :: ID -> String -> HUBIGraph Int
vertexFontfamily node family = setVertexAttribute node "fontfamily" family

vertexFontsize :: ID -> Int -> HUBIGraph Int
vertexFontsize node sz = setVertexAttribute node "fontsize" $ show sz

vertexVisible :: ID -> Bool -> HUBIGraph Int
vertexVisible node bl = setVertexAttribute node "visible" $ map toLower (show bl)

setEdgeAttribute :: ID -> String -> String -> HUBIGraph Int
setEdgeAttribute eid name val =
    do serv <- asks server
       liftIO $ remote serv "ubigraph.set_edge_attribute" eid name val

edgeArrow :: ID -> Bool -> HUBIGraph Int
edgeArrow eid bl = setEdgeAttribute eid "arrow" $ map toLower (show bl)

edgeArrowPosition :: ID -> Float -> HUBIGraph Int
edgeArrowPosition eid pos = setEdgeAttribute eid "arrow_position" $ show pos

edgeArrowRadius :: ID -> Float -> HUBIGraph Int
edgeArrowRadius eid rad = setEdgeAttribute eid "arrow_radius" $ show rad

edgeArrowLength :: ID -> Float -> HUBIGraph Int
edgeArrowLength eid len = setEdgeAttribute eid "arrow_length" $ show len

edgeArrowReverse :: ID -> Bool -> HUBIGraph Int
edgeArrowReverse eid bl = setEdgeAttribute eid "arrow_reverse" $ map toLower (show bl)

edgeColor :: ID -> String -> HUBIGraph Int
edgeColor eid color = setEdgeAttribute eid "color" color

edgeLabel :: ID -> String -> HUBIGraph Int
edgeLabel eid str = setEdgeAttribute eid "label" str

edgeFontcolor :: ID -> String -> HUBIGraph Int
edgeFontcolor eid color = setEdgeAttribute eid "fontcolor" color

edgeFontfamily :: ID -> String -> HUBIGraph Int
edgeFontfamily eid family = setEdgeAttribute eid "fontfamily" family

edgeFontsize :: ID -> Int -> HUBIGraph Int
edgeFontsize eid sz = setEdgeAttribute eid "fontsize" $ show sz

edgeOriented :: ID -> Bool -> HUBIGraph Int
edgeOriented eid bl = setEdgeAttribute eid "oriented" $ map toLower (show bl)

edgeSpline :: ID -> Bool -> HUBIGraph Int
edgeSpline eid bl = setEdgeAttribute eid "spline" $ map toLower (show bl)

edgeShowstrain :: ID -> Bool -> HUBIGraph Int
edgeShowstrain eid bl = setEdgeAttribute eid "showstrain" $ map toLower (show bl)

edgeStroke :: ID -> Stroke -> HUBIGraph Int
edgeStroke eid stroke = setEdgeAttribute eid "stroke" $ map toLower (show stroke)

edgeStrength :: ID -> Float -> HUBIGraph Int
edgeStrength eid len = setEdgeAttribute eid "strength" $ show len

edgeVisible :: ID -> Bool -> HUBIGraph Int
edgeVisible eid bl = setEdgeAttribute eid "visible" $ map toLower (show bl)

edgeWith :: ID -> Float -> HUBIGraph Int
edgeWith eid width = setEdgeAttribute eid "width" $ show width
