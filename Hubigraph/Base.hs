module Hubigraph.Base (
   Hubigraph, Ubigraph(..), VertexID, EdgeID, Edge, Color, Shape(..), Stroke(..),
   Attr(..), VAttr(..), EAttr(..), StyleID,
  ) where

import Control.Monad.Reader (ReaderT(..), runReaderT, asks, liftIO, lift)
import Network.XmlRpc.Internals

type Hubigraph = ReaderT Ubigraph IO
data Ubigraph = Ubigraph { server :: String }

type VertexID = Int
type EdgeID   = Int
type StyleID  = Int
type Edge = (VertexID, VertexID)

type Color = String

data Shape = Cone | Cube | Dodecahedron | Icosahedron
            | Octahedron | Sphere | Tetrahedron
            | Torus deriving Show

data Stroke = Solid | Dashed | Dotted
            | None deriving Show

type VStyle = String
type EStyle = String

data VAttr = VColor Color
           | VShape Shape
           | VFontsize Int
           | VVisible Bool

data EAttr = EColor Color
           | EShape Shape
           | EStroke Stroke
           | EFontsize Int
           | EVisible Bool

class Attr a where
    toPair :: a -> (String, String)

instance Attr VAttr where
    toPair (VColor c)     = ("color",    c)
    toPair (VShape Cone)  = ("shape",    "cone")
    toPair (VShape Cube)  = ("shape",    "cube")
    toPair (VShape _)     = ("shape",    "___")
    toPair (VFontsize sz) = ("fontsize", show sz)

instance Attr EAttr where
    toPair (EColor c)     = ("color",    c)
    toPair (EShape Cone)  = ("shape",    "cone")
    toPair (EShape Cube)  = ("shape",    "cube")
    toPair (EShape _)     = ("shape",    "___")
    toPair (EFontsize sz) = ("fontsize", show sz)
