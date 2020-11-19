-- | THe web mouse api encodes the mouse state as a bitfield. 
-- | This module contains a few helper for working with those.
module Data.MouseButton
  ( MouseButton(..)
  , isPressed
  , nothingPressed
  ) where

import Prelude
import Data.Int.Bits (and)

data MouseButton
  = LeftMouseButton
  | RightMouseButton

-- This is here for nicer docs
type RawMouseState
  = Int

-- | Check if there's no button pressed.
nothingPressed :: RawMouseState -> Boolean
nothingPressed = (==) 0

-- | Check if a particular button is pressed.
isPressed :: MouseButton -> RawMouseState -> Boolean
isPressed button bits = bits `and` buttonCode /= 0
  where
  buttonCode = case button of
    LeftMouseButton -> 1
    RightMouseButton -> 2
