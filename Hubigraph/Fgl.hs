module Hubigraph.Fgl (
   addGraph,
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

addGraph :: Graph gr => gr a b -> Hubigraph ()
addGraph g = do
  mapM_ newV (nodes g)
  mapM_ newE (edges g)
