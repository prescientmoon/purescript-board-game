module Data.GameMap where

import Prelude
import Camera (Vec2)
import Data.Array as Array
import Data.Int (floor)
import Data.Typelevel.Num (d0, d1)
import Data.Vec (vec2, (!!))
import Math (sqrt)

newtype BackgroundMap
  = BackgroundMap
  { width :: Int
  , height :: Int
  , url :: String
  }

data CellData
  = EmptyCell
  | GamePiece

newtype GameMap
  = GameMap (Array (Array CellData))

generateMap :: Vec2 Int -> GameMap
generateMap size =
  GameMap
    $ Array.replicate (size !! d0)
    $ Array.replicate (size !! d1) EmptyCell

maximumMapSize :: Number -> Vec2 Number -> Vec2 Int
maximumMapSize cellSize windowSize = vec2 width height
  where
  width = floor $ (windowSize !! d0) / (2.0 * cellSize)

  height = floor $ (windowSize !! d1) / (sqrt 3.0 * cellSize)
