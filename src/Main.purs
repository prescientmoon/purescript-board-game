module Main where

import Prelude
import App.Board as Board
import Camera as Camera
import Effect (Effect)
import GameMap (BackgroundMap(..))
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI Board.component input body
  where
  input :: Board.Input
  input =
    { backgroundMap:
      BackgroundMap
        { url: "romania.svg"
        , height: 1000
        , width: 1000
        }
    , camera: Camera.defaultCamera
    , lastMousePosition: zero
    }
