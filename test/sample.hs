{-# OPTIONS -fglasgow-exts #-}

import Hubigraph
import Control.Monad.Reader (runReaderT, liftIO)
import Foreign
import Foreign.C
import System.Posix.Signals

foreign import ccall "sleep" sleep :: Int -> IO ()

main :: IO ()
main = initHubigraph "http://192.168.0.2:20738/RPC2" >>= runReaderT run

run :: Hubigraph ()
run = test1 >> io (sleep 5) >> clear >>
      test2 >> io (sleep 5)

test1 :: Hubigraph ()
test1 = do
  mapM newVertex' [0.. (num-1)]
  mapM (newEdge' num) [0..(num-1)]
  return ()
      where num = 20 :: Int
            newEdge' upper node
                = newEdgeWithID node (node, (node+1) `mod` upper)
            newVertex' n = do newVertexWithID n
                              vertexLabel n $ show n

test2 :: Hubigraph ()
test2 = do
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

io :: IO a -> Hubigraph a
io = liftIO
