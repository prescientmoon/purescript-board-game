-- | Implementation of a virtual camera used for zooming and panning.
module Camera where

import Prelude
import Data.Lens (Lens', over, view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple, uncurry)
import Data.Typelevel.Num (D2, d0, d1)
import Data.Vec (Vec, vec2, (!!))
import Halogen.HTML (IProp)
import Halogen.Svg.Attributes as SA

type Vec2
  = Vec D2

newtype Camera
  = Camera
  { position :: Vec2 Number
  , zoom :: Number
  , zoomBounds :: Tuple Number Number
  }

derive instance newtypeCamera :: Newtype Camera _

defaultCamera :: Tuple Number Number -> Camera
defaultCamera zoomBounds =
  Camera
    { position: zero
    , zoom: 1.0
    , zoomBounds
    }

toWorldCoordinates :: Camera -> Vec2 Number -> Vec2 Number
toWorldCoordinates (Camera { position, zoom }) vec = position + ((_ / zoom) <$> vec)

origin :: Camera -> Vec2 Number
origin = view _CameraPosition

-- Zoom on the origin of the __screen__ view 
zoomOrigin :: Number -> Camera -> Camera
zoomOrigin amount (Camera camera@{ zoomBounds, zoom }) =
  Camera
    camera
      { zoom = uncurry clamp zoomBounds $ amount * zoom
      }

-- Translate the camera using a vector in world coordinates
-- Translate the camera using a vector in screen coordinates
screenPan :: Vec2 Number -> Camera -> Camera
screenPan vector camera = over _CameraPosition (_ - worldCoordinates) camera
  where
  worldCoordinates = toWorldCoordinates camera vector - origin camera

constrainPosition :: Vec2 Number -> Vec2 Number -> Camera -> Camera
constrainPosition minimum maximum =
  over _CameraPosition \position ->
    vec2
      (clamp (minimum !! d0) (maximum !! d0) (position !! d0))
      (clamp (minimum !! d1) (maximum !! d1) (position !! d1))

-- Zooms relative to a poin in screen coorinates
zoomOn :: Vec2 Number -> Number -> Camera -> Camera
zoomOn point amount = screenPan (-point) >>> zoomOrigin amount >>> screenPan point

-- Generate a svg viewbox from a Camera
toViewBox :: forall r i. Vec2 Number -> Camera -> IProp ( viewBox ∷ String | r ) i
toViewBox scale (Camera { position, zoom }) =
  SA.viewBox (position !! d0)
    (position !! d1)
    (scale !! d0 / zoom)
    (scale !! d1 / zoom)

-- Lenses
_CameraPosition :: Lens' Camera (Vec2 Number)
_CameraPosition = _Newtype <<< prop (SProxy :: _ "position")

_CameraZoom :: Lens' Camera Number
_CameraZoom = _Newtype <<< prop (SProxy :: _ "zoom")

_CameraZoomBounds :: Lens' Camera (Tuple Number Number)
_CameraZoomBounds = _Newtype <<< prop (SProxy :: _ "zoomBounds")
