module App.Board where

import Prelude
import Camera (Camera, Vec2, toViewBox)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.Symbol (SProxy(..))
import Data.Vec (vec2)
import Effect.Class (class MonadEffect)
import GameMap (BackgroundMap(..))
import Halogen (AttrName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE

data MouseState
  = MouseUp
  | MouseDown
    { lastMousePosition :: Vec2 Number
    }

type State
  = { backgroundMap :: BackgroundMap
    , mouseState :: MouseState
    , camera :: Camera
    }

data Action
  = Initialize

type Input
  = State

_rawHtml :: SProxy "rawHtml"
_rawHtml = SProxy

gameMap :: H.RefLabel
gameMap = H.RefLabel "gameMap"

component :: forall q o m. MonadEffect m => H.Component HH.HTML q Input o m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval:
      H.mkEval
        $ H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            }
    }

-- <object type="image/svg+xml" data="image.svg"></object>
render :: forall m cs. MonadEffect m => State -> H.ComponentHTML Action cs m
render state =
  SE.svg
    [ SA.height 1000.0
    , SA.width 1000.0
    , HP.id_ "board"
    , toViewBox (vec2 1000.0 1000.0) state.camera
    ]
    [ backgroundMap state.backgroundMap ]

backgroundMap :: forall m cs. BackgroundMap -> H.ComponentHTML Action cs m
backgroundMap (BackgroundMap { width, height, url }) =
  SE.foreignObject
    [ SA.width $ toNumber width
    , SA.height $ toNumber width
    ]
    [ HH.object
        [ HP.type_ (MediaType "image/svg+xml")
        , HP.attr (AttrName "data") ("assets/" <> url)
        , HP.width width
        , HP.height height
        ]
        []
    ]

handleAction :: forall cs o m. MonadEffect m => Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Initialize -> pure unit
