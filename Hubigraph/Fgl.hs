module Hubigraph.Fgl (
   addGraph, pathVertexColor,
  ) where

import Data.Graph.Inductive
import Data.Graph.Inductive.Graph as G

import Hubigraph

mkEdgeId :: G.Edge -> Int
mkEdgeId (src,dst) = src * 10000 + dst

newE :: G.Edge -> Hubigraph Int
newE e = newEdgeWithID (mkEdgeId e) e

newV :: G.Node -> Hubigraph Int
newV v = newVertexWithID v

pathEdges :: Path -> [G.Edge]
pathEdges (s:d:xs) = (s,d):pathEdges xs
pathEdges (s:[]) = []
pathEdges []     = []

addGraph :: Graph gr => gr a b -> Hubigraph ()
addGraph g = do
  mapM_ newV (nodes g)
  mapM_ newE (edges g)

pathVertexColor :: Path -> Color -> Hubigraph ()
pathVertexColor p c = mapM_ (\n -> vertexColor n c) p

pathEdgeColor :: Path -> Color -> Hubigraph ()
pathEdgeColor p c = undefined

pathWidth :: Path -> Float -> Hubigraph ()
pathWidth p width = mapM_ (\n -> edgeWidth (mkEdgeId n) width) $ pathEdges p
