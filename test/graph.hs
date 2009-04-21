import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.Query.SP ( spLength )

import Hubigraph
import Hubigraph.Fgl (addGraph)

main = do
  let nodes = [ (1,"Tokyo"), (2,"Nagoya"), (3,"Osaka"),
                (4,"Kyoto"), (5,"Hiroshima"), (6,"Hakata") ]
      edges = [ (1,2,9), (1,3,90), (2,3,8), (2,4,9),
                (3,5,80), (4,6,20), (4,5,10) ]
      graph = G.mkGraph nodes edges :: G.Gr String Int
  initHubigraph serv >>= runHubigraph (addGraph graph)
      where
        serv = "http://leopard166:20738/RPC2"