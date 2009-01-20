module Hubigraph.Base (
   Hubigraph, Ubigraph(..), ID, Edge, Shapes(..), Stroke(..),
  ) where

import Control.Monad.Reader (ReaderT(..), runReaderT, asks, liftIO, lift)

type Hubigraph = ReaderT Ubigraph IO
data Ubigraph = Ubigraph { server :: String }

type ID = Int
type Edge = (Int, Int)
data Shapes = Cone | Cube | Dodecahedron | Icosahedron
            | Octahedron | Sphere | Tetrahedron
            | Torus deriving (Show)
data Stroke = Solid | Dashed | Dotted
            | None deriving (Show)
