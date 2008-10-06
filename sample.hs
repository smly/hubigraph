import HUBIGraph

-- example
num = 20::Int
test1 = do
  mapM addNode [0.. (num-1)]
  mapM (addEdge' num) [0..(num-1)]
      where addEdge' upper node
                = addEdge node (node, (node+1) `mod` upper)

-- example with labels
test2 = do
  mapM addNode' [0..(num-1)]
  mapM (addEdge' num) [0..(num-1)]
      where addEdge' upper node
                = addEdge node (node, (node+1) `mod` upper)
            addNode' n = do addNode n
                            vertexLabel n $ show n
