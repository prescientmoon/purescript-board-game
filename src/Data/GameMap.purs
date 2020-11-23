-- | Data structure for the hexagonal maps
-- | Based on https://www.redblobgames.com/grids/hexagons
module Data.GameMap where

import Prelude

import Data.Array as Array
import Data.Int (even, floor, toNumber)
import Data.Lens (Traversal')
import Data.Lens.Index (ix)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Newtype (class Newtype)
import Data.Typelevel.Num (d0, d1)
import Data.Vec (vec2, (!!))
import Data.Vec2 (Vec2)
import Halogen.Svg.Attributes (Color)
import Math (sqrt)
import Math as Math

-- Right now this is more of a placeholder and only carries a color around.
type Piece
  = { color :: Color }

-- A cell is just a hexagon in the grid. 
-- Right now cells only hold a stack of pieces.
type Cell
  = Array Piece

-- | A GameMap is a odd-q hexagonal grid
newtype GameMap
  = GameMap (Array (Array Cell))

-- | The coordinates on the map a piece sits at.
type PieceCoordinates
  = { x :: Int, y :: Int, index :: Int }

-- | Generate a map of a given size.
generateMap :: Vec2 Int -> GameMap
generateMap size =
  GameMap
    $ Array.replicate (size !! d0)
    $ Array.replicate (size !! d1) []

-- | Calculuate the maximum map size fitting in an arbitrary rect.
maximumMapSize :: Number -> Vec2 Number -> Vec2 Int
maximumMapSize cellSize size = vec2 width height
  where
  width = floor $ (size !! d0) / (2.0 * cellSize)

  height = floor $ (size !! d1) / (sqrt 3.0 * cellSize)

-- | Get the coordinates of all the neighbours a cell has.
neighbours :: Vec2 Int -> Array (Vec2 Int)
neighbours position = directions <#> (position + _)
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

-- | Calculates the position of the nth corner of a hexaon
hexagonCorner :: Number -> Int -> Vec2 Number -> Vec2 Number
hexagonCorner size nth center =
  center
    + vec2
        (size * Math.cos angle)
        (size * Math.sin angle)
  where
  angle = toNumber nth * Math.pi / 3.0

-- Helper lens for focusing on a single hexagon
_cell :: Int -> Int -> Traversal' GameMap Cell
_cell x y = _Newtype <<< ix x <<< ix y

-- Typeclass instances
derive instance newtypeGameMap :: Newtype GameMap _
