import Hubigraph

call :: Hubigraph () -> IO ()
call cmd = initHubigraph serv >>= runHubigraph cmd
    where serv = "http://localhost:20738/RPC2"
