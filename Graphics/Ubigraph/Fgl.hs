{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Hubigraph.Fgl (
   addGraph, pathVertexColor,
  ) where

import Data.Graph.Inductive
import Data.Graph.Inductive.Graph

import qualified Hubigraph as H

mkEdgeId :: Edge -> Int
mkEdgeId (src,dst) = src * 10000 + dst

newE :: Edge -> H.Hubigraph Int
newE e = H.newEdgeWithID (mkEdgeId e) e

newV :: LNode' a => LNode a -> H.Hubigraph ()
newV ln = do
  H.newVertexWithID $ fst ln
  H.vertexLabel (fst ln) $ print' ln
  return ()

pathEdges :: Path -> [Edge]
pathEdges (s:d:xs) = (s,d):pathEdges xs
pathEdges (s:[]) = []
pathEdges []     = []

addGraph :: LNode' a => Graph gr => gr a b -> H.Hubigraph ()
addGraph g = do
  mapM_ newV (labNodes g)
  mapM_ newE (edges g)

pathVertexColor :: Path -> H.Color -> H.Hubigraph ()
pathVertexColor p c = mapM_ (\n -> H.vertexColor n c) p

pathEdgeColor :: Path -> H.Color -> H.Hubigraph ()
pathEdgeColor p c = undefined

pathWidth :: Path -> Float -> H.Hubigraph ()
pathWidth p width = mapM_ (\n -> H.edgeWidth (mkEdgeId n) width) $ pathEdges p

class LNode' a where
    print' :: LNode a -> String
instance LNode' String where
    print' (n,l) = l
instance LNode' Int where
    print' (n,l) = show l
instance LNode' Double where
    print' (n,l) = show l
instance LNode' a  where
    print' (n,l) = ""
