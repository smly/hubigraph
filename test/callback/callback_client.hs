import Hubigraph
r x = initHubigraph "http://localhost:20738/RPC2" >>= runHubigraph x
main = do
  mapM_ (r . newVertexWithID) [1..3]
  mapM_ (r . (\n->setVAttr n (VCallback url))) [1..3]
       where url = "http://localhost:19999/test_server.bin/vertex_callback"