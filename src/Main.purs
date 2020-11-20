module Main where

import Prelude
import App.Board as Board
import Data.GameMap (BackgroundMap(..))
import Data.Vec (vec2)
import Effect (Effect)
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
        , height: 500
        , width: 1000
        }
    , cellSize: 50.0
    , mapPadding: vec2 100.0 200.0
    }
