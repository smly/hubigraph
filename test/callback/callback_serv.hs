import Hubigraph
import Network.XmlRpc.Server

draw :: Int -> IO Int
draw a =
    do
      initHubigraph "http://localhost:20738/RPC2" >>= runHubigraph (setVAttr a (VColor "#ff0000"))
      return 0

main = cgiXmlRpcServer [("vertex_callback", fun draw)]
