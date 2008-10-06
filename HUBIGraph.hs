module HUBIGraph (
   addNode, removeNode, addNodeWithoutID,
   addEdge, removeEdge, addEdgeWithoutID,
   vertexColor, vertexShape, vertexShapedetail,
   vertexLabel, vertexSize, vertexFontcolor,
   vertexFontfamily, vertexFontsize, vertexVisible,
   edgeColor, edgeLabel, edgeFontcolor,
   edgeFontfamily, edgeFontsize, edgeSpline,
   edgeStrength, edgeOriented, edgeStroke,
   edgeArrow, edgeArrowPosition,
   edgeArrowRadius, edgeArrowLength, edgeArrowReverse,
   edgeShowstrain, edgeVisible, edgeWith,
  ) where

import Network.XmlRpc.Client
import Char (toLower)
import Prelude hiding (catch)

type ID = Int
type Edge = (Int, Int)
data Shapes = Cone | Cube | Dodecahedron | Icosahedron
            | Octahederon | Sphere | Octahedron
            | Torus deriving (Show)
data Stroke = Solid | Dashed | Dotted
            | None deriving (Show)

serv :: String
serv = "http://localhost:20738/RPC2"

clear :: IO Int
clear = remote serv "ubigraph.clear"

addNode :: ID -> IO Int
addNode node = remote serv "ubigraph.new_vertex_w_id" node

addNodeWithoutID :: IO Int
addNodeWithoutID = remote serv "ubigraph.new_vertex"

removeNode :: ID -> IO Int
removeNode node = remote serv "ubigraph.remove_vertex" node

addEdge :: ID -> Edge -> IO Int
addEdge eid (src,dst) = remote serv "ubigraph.new_edge_w_id" eid src dst

addEdgeWithoutID :: Edge -> IO Int
addEdgeWithoutID (src,dst) = remote serv "ubigraph.new_edge" src dst

removeEdge :: ID -> IO Int
removeEdge eid = remote serv "ubigraph.remove_edge" eid

{- === VERTEX STYLE ATTRIBUTES ====================
   color, shape, shapedetail, label, labelpos,
   size, fontcolor, fontfamily, fontsize, visible
-}
vattr :: ID -> String -> String -> IO Int
vattr id op val = remote serv "ubigraph.set_vertex_attribute" id op val

vertexShape :: ID -> Shapes -> IO Int
vertexShape node shape = vattr node "shape" $ map toLower (show shape)

-- sensible value from 5 to 40
vertexShapedetail :: ID -> Int -> IO Int
vertexShapedetail node val = vattr node "shapedetail" $ show val

vertexColor :: ID -> String -> IO Int
vertexColor node color = vattr node "color" color

vertexSize :: ID -> Float -> IO Int
vertexSize node sz = vattr node "size" $ show sz

vertexLabel :: ID -> String -> IO Int
vertexLabel node str = vattr node "label" str

vertexFontcolor :: ID -> String -> IO Int
vertexFontcolor node color = vattr node "fontcolor" color

vertexFontfamily :: ID -> String -> IO Int
vertexFontfamily node family = vattr node "fontfamily" family

vertexFontsize :: ID -> Int -> IO Int
vertexFontsize node sz = vattr node "fontsize" $ show sz

vertexVisible :: ID -> Bool -> IO Int
vertexVisible node bl = vattr node "visible" $ map toLower (show bl)

{- === EDGE STYLE ATTRIBUTES ====================
   color, label, fontcolor, fontfamily, fontsize,
   spline, strength, oriented, stroke, width,
   arrow, showstrain, visible
-}

-- set edge attributes
eattr :: ID -> String -> String -> IO Int
eattr eid name val = remote serv "ubigraph.set_edge_attribute" eid name val

edgeArrow :: ID -> Bool -> IO Int
edgeArrow eid bl = eattr eid "arrow" $ map toLower (show bl)

edgeArrowPosition :: ID -> Float -> IO Int
edgeArrowPosition eid pos = eattr eid "arrow_position" $ show pos

edgeArrowRadius :: ID -> Float -> IO Int
edgeArrowRadius eid rad = eattr eid "arrow_radius" $ show rad

edgeArrowLength :: ID -> Float -> IO Int
edgeArrowLength eid len = eattr eid "arrow_length" $ show len

edgeArrowReverse :: ID -> Bool -> IO Int
edgeArrowReverse eid bl = eattr eid "arrow_reverse" $ map toLower (show bl)

edgeColor :: ID -> String -> IO Int
edgeColor eid color = eattr eid "color" color

edgeLabel :: ID -> String -> IO Int
edgeLabel eid str = eattr eid "label" str

edgeFontcolor :: ID -> String -> IO Int
edgeFontcolor eid color = vattr eid "fontcolor" color

edgeFontfamily :: ID -> String -> IO Int
edgeFontfamily eid family = vattr eid "fontfamily" family

edgeFontsize :: ID -> Int -> IO Int
edgeFontsize eid sz = vattr eid "fontsize" $ show sz

edgeOriented :: ID -> Bool -> IO Int
edgeOriented eid bl = eattr eid "oriented" $ map toLower (show bl)

edgeSpline :: ID -> Bool -> IO Int
edgeSpline eid bl = eattr eid "spline" $ map toLower (show bl)

edgeShowstrain :: ID -> Bool -> IO Int
edgeShowstrain eid bl = eattr eid "showstrain" $ map toLower (show bl)

edgeStroke :: ID -> Stroke -> IO Int
edgeStroke eid stroke = eattr eid "stroke" $ map toLower (show stroke)

edgeStrength :: ID -> Float -> IO Int
edgeStrength eid len = eattr eid "strength" $ show len

edgeVisible :: ID -> Bool -> IO Int
edgeVisible eid bl = eattr eid "visible" $ map toLower (show bl)

edgeWith :: ID -> Float -> IO Int
edgeWith eid width = eattr eid "width" $ show width
