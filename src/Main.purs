module Main where

import Prelude
import App.Board as Board
import Data.Time.Duration (Milliseconds(..))
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
    { backgroundUrl: "romania.svg"
    , backgroundSize: vec2 1000.0 500.0
    , cellSize: 50.0
    , mapPadding: vec2 50.0 100.0
    , windowSize: vec2 700.0 700.0
    , stackedPieceOffset: 4.0
    , borderScrollAreaSize: 100.0
    , borderScrollSpeed: 20.0
    , borderScrollInterval: Milliseconds 50.0
    }
