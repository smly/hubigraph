-- import Data.Graph
import Control.Monad
import Data.Char (isSpace)
import Data.Graph

import Hubigraph.Graph

main :: IO ()
main =
    do
      gdata <- liftM lines (readFile"sample2_matrix.txt")
      graph <- buildG' gdata
      initHubigraph serv >>= runHubigraph (test2 graph)
    where
      serv = "http://127.0.0.1:20738/RPC2"

      mkRange :: String -> (Int, Int)
      mkRange str = (\(a,b)->(read a::Int,read b::Int)) $ break isSpace str

      buildG' gdata = return $ buildG b e
          where b = (mkRange (head gdata))
                e = (map mkRange (tail gdata))

test2 :: Graph -> Hubigraph ()
test2 g =
    do
      addGraph g eid vid
      mapM_ (\n -> edgeArrow (snd n) True) (zip (edges g) (iterate (+1) eid))
    where eid = 0
          vid = 0

