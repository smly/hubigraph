# Hubigraph

http://ooxo.org/hubigraph/

## DESCRIPTION

a Haskell wrap for Ubigraph (http://www.ubietylab.net/ubigraph)

## FEATURES/PROBLEMS

It provides a shortcut to draw a graph in ubigraph (by calling XML-RPC internally.)

## SYNOPSIS

Make sure Ubigraph server is started before using this library.

     import Graphics.Ubigraph

     u x = initHubigraph "http://localhost:20738/RPC2" >>= runHubigraph x

     main = u $ mkRing 10

     mkRing n = do mapM_ (newVertexWithID) [0..(n-1)]
                   mapM_ (newEdge') [0..(n-1)]
                   sid <- newVStyle 0
                   setVStyleAttr (VColor "#ff0000") sid
                   setVStyleAttr (VShape Sphere) sid
                   mapM_ (changeVStyle sid) [0..(n-1)]
         where newEdge' e = newEdge (e, (e+1) `mod` n)

## REQUIREMENTS

Ubigraph (http://www.ubietylab.net/ubigraph).

## INSTALL

$ cabal install hubigraph

## LICENSE

The BSD3 License
