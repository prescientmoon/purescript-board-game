-- | Implementation of a virtual camera used for zooming and panning.
module Camera where

import Prelude

import Data.Lens (Lens', over, view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple, uncurry)
import Data.Typelevel.Num (d0, d1)
import Data.Vec ((!!))
import Data.Vec2 (Vec2)
import Halogen.HTML (IProp)
import Halogen.Svg.Attributes as SA


-- | Virtual camera structure.
newtype Camera
  = Camera
  -- | The world coordinates the top-left corner of the screen is at.  
  { position :: Vec2 Number
  -- | The amount to scale the scene by
  , zoom :: Number
  -- | Minimum and maximum values the zoom is constrained between.
  , zoomBounds :: Tuple Number Number
  }

derive instance newtypeCamera :: Newtype Camera _

-- | Creates a camera with no transform.
defaultCamera :: Tuple Number Number -> Camera
defaultCamera zoomBounds =
  Camera
    { position: zero
    , zoom: 1.0
    , zoomBounds
    }

-- | Transforms a position in screen coordinates to one in world coordinates.
toWorldCoordinates :: Camera -> Vec2 Number -> Vec2 Number
toWorldCoordinates (Camera { position, zoom }) vec = position + ((_ / zoom) <$> vec)

-- | Scale everything in the scene in such a way the origin of the coordinates remains fixed.
zoomOrigin :: Number -> Camera -> Camera
zoomOrigin amount (Camera camera@{ zoomBounds, zoom }) =
  Camera
    camera
      { zoom = uncurry clamp zoomBounds $ amount * zoom
      }

-- | Zoom in such a way a particular point on the screen remains fixed.
-- | The point in question is usually either the center of the screen or the mouse.
zoomOn :: Vec2 Number -> Number -> Camera -> Camera
zoomOn point amount =
  screenPan (-point) -- make the point we want to zoom on the origin
    >>> zoomOrigin amount -- zoom on the origin
    >>> screenPan point -- move the origin back to where it was before zooming

-- | Move the origin by an amount in world coordinates
pan :: Vec2 Number -> Camera -> Camera
pan vec = over _CameraPosition (_ - vec)

-- | Move the origin by an amount in screen coordinates
screenPan :: Vec2 Number -> Camera -> Camera
screenPan vector camera = pan worldCoordinates camera
  where
  -- We splify transform the screen coordinates to world coordinates and then apply the normal pan
  worldCoordinates = toWorldCoordinates camera vector - view _CameraPosition camera

-- | Generate the viewBox attribute for svg elements.
toViewBox :: forall r i. Vec2 Number -> Camera -> IProp ( viewBox ∷ String | r ) i
toViewBox size (Camera { position, zoom }) =
  SA.viewBox (position !! d0)
    (position !! d1)
    (size !! d0 / zoom)
    (size !! d1 / zoom)

-- Lenses
_CameraPosition :: Lens' Camera (Vec2 Number)
_CameraPosition = _Newtype <<< prop (SProxy :: _ "position")

_CameraZoom :: Lens' Camera Number
_CameraZoom = _Newtype <<< prop (SProxy :: _ "zoom")

_CameraZoomBounds :: Lens' Camera (Tuple Number Number)
_CameraZoomBounds = _Newtype <<< prop (SProxy :: _ "zoomBounds")
