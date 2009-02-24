import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.Query.SP ( spLength )

import Hubigraph
import Hubigraph.Base

type Graph = G.Gr String Int

addGraph :: Graph -> Hubigraph ()
addGraph g = do
  mapM_ newV (G.labNodes g)
  mapM_ newE (G.labEdges g)
    where
      newV :: (G.LNode String) -> Hubigraph Int
      newV (id,l) = do
               newVertexWithID id
               vertexLabel id l
      newE (s,d,_) = do
               newEdge (s,d)
               --newEdgeWithID $ (s,d)
               -- use State monad for 
               -- LEdge Int -> EdgeId hashing

main = do
  let nodes = [ (1,"Tokyo"), (2,"Nagoya"), (3,"Osaka"),
                (4,"Kyoto"), (5,"Hiroshima"), (6,"Hakata") ]
      edges = [ (1,2,9), (1,3,90), (2,3,8), (2,4,9),
                (3,5,80), (4,6,20), (4,5,10) ]
      graph = G.mkGraph nodes edges :: G.Gr String Int
      spl   = spLength 1 6 graph
  putStrLn $ "spLength: " ++ show spl
  initHubigraph serv >>= runHubigraph (run graph)
      where
        serv = "http://localhost:20738/RPC2"

run graph =  do
  addGraph graph
  vertexColor 1 "#ff0000"
  vertexColor 6 "#ffff00"
