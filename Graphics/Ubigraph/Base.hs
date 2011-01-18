module Graphics.Ubigraph.Base (
   Hubigraph, Ubigraph(..), VertexID, EdgeID, Edge, Color, Shape(..), Stroke(..),
   Attr(..), VAttr(..), EAttr(..), StyleID,
   runHubigraph, initHubigraph,
  ) where

import Control.Monad.Reader (ReaderT(..), runReaderT, asks, liftIO, lift)

runHubigraph = runReaderT

initHubigraph :: (Monad m) => String -> m Ubigraph
initHubigraph serv = return ( Ubigraph { server = serv } )

toBool :: IO Int -> IO Bool
toBool x = do
  x' <- x
  return $ if x' == 0 then True else False

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
           | VShapedetail Int
           {- only meaningful for sphere, cone, torus
            Useful range 4-40. Default: 0 (auto-adjust) -}
           | VLabel String
           | VSize Float
           | VFontcolor Color
           | VFontfamily String
           | VFontsize Int
           | VVisible Bool
           | VCallback String

data EAttr = EColor Color
           | ELabel String
           | EFontcolor Color
           | EFontfamily String
           | EFontsize Int
           | ESpline Bool
           | EStrength Float
           {- Default: 1.0, Use 0.0 for edge
              that do not affect layout -}
           | EOriented Bool
           | EStroke Stroke
           | EWidth Float -- default: 1.0
           | EArrow Bool -- default: false
           | EArrowPosition Float -- default 0.5
           | EArrowRadius Float -- default 1.0
           | EArrowLength Float -- default 1.0
           | EArrowReverse Bool -- default false
           | EShowstrain Bool -- default: false
           | EVisible Bool

class Attr a where
    toPair :: a -> (String, String)

instance Attr VAttr where
    toPair (VColor c)       = ("color", c)
    toPair (VShape a)       = ("shape", show a)
    toPair (VShapedetail i) = ("shapedetail", show i)
    toPair (VLabel s)       = ("label", s)
    toPair (VSize f)        = ("size", show f)
    toPair (VFontcolor c)   = ("fontcolor", c)
    toPair (VFontfamily s)  = ("fontfamily", s)
    toPair (VFontsize sz)   = ("fontsize", show sz)
    toPair (VVisible b)     = ("visible", show b)
    toPair (VCallback url)  = ("callback_left_doubleclick", url)

instance Attr EAttr where
    toPair (EColor c)     = ("color", c)
    toPair (ELabel s)     = ("label", s)
    toPair (EFontcolor c) = ("fontcolor", c)
    toPair (EFontsize sz) = ("fontsize", show sz)
    toPair (ESpline b)    = ("spline", show b)
    toPair (EStrength f)  = ("strength", show f)
    toPair (EOriented b)  = ("oriented", show b)
    toPair (EStroke st)   = ("stroke", show st)
    toPair (EWidth f)     = ("width", show f)
    toPair (EArrow b)     = ("arrow", show b)
    toPair (EArrowPosition f)     = ("arrow_position", show f)
    toPair (EArrowRadius f)     = ("arrow_radius", show f)
    toPair (EArrowLength f)     = ("arrow_length", show f)
    toPair (EArrowReverse b)     = ("arrow_reverse", show b)
    toPair (EShowstrain b)= ("showstrain", show b)
    toPair (EVisible b)   = ("visible", show b)

