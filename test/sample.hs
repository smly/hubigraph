import Hubigraph

main :: IO ()
main = initHubigraph "http://127.0.0.1:20738/RPC2" >>= runHubigraph test

test :: Hubigraph ()
test = do
  mapM newVertex' [0..(num-1)]
  mapM (newEdge' num) [0..(num-1)]
  return ()
      where num = 20 :: Int
            newEdge' upper node
                = newEdgeWithID node (node, (node+1) `mod` upper)
            newVertex' n = do newVertexWithID n
                              vertexLabel n $ show n
                              vertexShape n Icosahedron
                              vertexSize n 3.0
                              vertexColor n "#ff0000"
