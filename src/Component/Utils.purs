-- | Utility functions for use in halogen components.
module Component.Utils where

import Prelude

import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe, maybe)
import Data.String (joinWith)
import Data.Vec (vec2)
import Data.Vec2 (Vec2)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Web.UIEvent.MouseEvent as MouseEvent

-- | Conditional rendering helper
whenElem :: forall p i. Boolean -> (Unit -> HTML p i) -> HTML p i
whenElem condition f = if condition then f unit else HH.text ""

-- | Unwrap some html from a maybe
maybeElement :: forall p i a. Maybe a -> (a -> HTML p i) -> HTML p i
maybeElement x mkHtml  = maybe (HH.text "") mkHtml x 

-- | There is no classes propriety for svg elements so we have to join the classes manually
joinClasses :: Array (Maybe String) -> String
joinClasses = Array.catMaybes >>> joinWith " "

-- | Get the mouse position from a mouse event as a vector
getMousePosition :: MouseEvent.MouseEvent -> Vec2 Number
getMousePosition event = toNumber <$> vec2 (MouseEvent.clientX event) (MouseEvent.clientY event)