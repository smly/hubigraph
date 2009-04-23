import Control.Monad
import Data.Char (isSpace)

import Hubigraph
import Hubigraph.Fgl
import qualified Data.Graph.Inductive as G
import qualified Data.Graph.Inductive.Graph as GG

serv = "http://leopard166:20738/RPC2"

main :: IO ()
main = do
--  graphData <- liftM lines (readFile"test/MontrealMetro.txt")
  graphData <- liftM lines (readFile"test/LondonUnderground.txt")
  let nodeNames = zip (takeWhile (not.null) graphData) (iterate (+1) 1)
      edgepairs = map (convEdge nodeNames) (mkEdgePairs graphData)
      graphNode = map (\(x,y) -> (y,x)) nodeNames
      edgepair  = map (\(s,d) -> (s,d,"")) edgepairs
      g = (GG.mkGraph graphNode edgepair) :: G.Gr String String
  initHubigraph serv >>= runHubigraph (addGraph g >> return ())

c = initHubigraph serv >>= runHubigraph clear

mkEdgePairs :: [String] -> [(String, String)]
mkEdgePairs g = map (parsePair.(break (==','))) (tail $ dropWhile (not.null) g)
    where
      parsePair :: (String, String) -> (String, String)
      parsePair (x,y:yn:ys) = ((tail.tail) x, fst (break (==',') ys))

convEdge :: [(String, Int)] -> (String, String) -> (Int, Int)
convEdge m (x,y) =
    case lookup x m of
      Nothing -> error "invalid graph data"
      Just a  -> case lookup y m of
                   Nothing -> error "invalid graph data"
                   Just b  -> (a, b)
