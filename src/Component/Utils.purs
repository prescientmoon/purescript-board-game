module Component.Utils where

import Prelude
import Data.Maybe (Maybe, maybe)
import Halogen.HTML (HTML)
import Halogen.HTML as HH

-- Conditional rendering helper
whenElem :: forall p i. Boolean -> (Unit -> HTML p i) -> HTML p i
whenElem condition f = if condition then f unit else HH.text ""

-- Unwrap some html from a maybe
maybeElement :: forall p i a. Maybe a -> (a -> HTML p i) -> HTML p i
maybeElement = flip $ maybe $ HH.text ""
