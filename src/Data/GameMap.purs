module Data.GameMap where

import Prelude
import Camera (Vec2)
import Data.Array as Array
import Data.Int (even, floor)
import Data.Newtype (class Newtype)
import Data.Typelevel.Num (d0, d1)
import Data.Vec (vec2, (!!))
import Halogen.Svg.Attributes (Color)
import Math (sqrt)

type Piece
  = { color :: Color }

type Cell
  = Array Piece

newtype GameMap
  = GameMap (Array (Array Cell))

generateMap :: Vec2 Int -> GameMap
generateMap size =
  GameMap
    $ Array.replicate (size !! d0)
    $ Array.replicate (size !! d1) []

maximumMapSize :: Number -> Vec2 Number -> Vec2 Int
maximumMapSize cellSize windowSize = vec2 width height
  where
  width = floor $ (windowSize !! d0) / (2.0 * cellSize)

  height = floor $ (windowSize !! d1) / (sqrt 3.0 * cellSize)

neighbours :: Vec2 Int -> Array (Vec2 Int)
neighbours position = directions <#> (_ + position)
  where
  directions
    | even (position !! d0) = evenDirections
    | otherwise = evenDirections <#> map negate

  evenDirections =
    [ vec2 1 0
    , vec2 1 (-1)
    , vec2 0 (-1)
    , vec2 (-1) (-1)
    , vec2 (-1) 0
    , vec2 0 1
    ]

type PiecePosition
  = { x :: Int, y :: Int, index :: Int }

-- Typeclass instances
derive instance newtypeGameMap :: Newtype GameMap _
