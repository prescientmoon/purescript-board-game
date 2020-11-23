-- | The web mouse api encodes the mouse state as a bitfield. 
-- | This module contains a few helpers for working with those.
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

-- | The button state provided by the browser.
type RawMouseState = Int

-- | Check if there's no button pressed.
nothingPressed :: RawMouseState -> Boolean
nothingPressed = (==) 0

-- | Check if a particular button is pressed.
isPressed :: MouseButton -> RawMouseState -> Boolean
isPressed button bits = bits `and` buttonCode /= 0
  where
  -- Here we convert from our own representation to the browser one.
  buttonCode = case button of
    LeftMouseButton -> 1
    RightMouseButton -> 2
