module Component.Utils where

import Prelude
import Control.Monad.Rec.Class (forever)
import Data.Maybe (Maybe, maybe)
import Data.Time.Duration (Milliseconds)
import Effect.Aff (delay, error, forkAff, killFiber)
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.Query.EventSource as ES

-- Conditional rendering helper
whenElem :: forall p i. Boolean -> (Unit -> HTML p i) -> HTML p i
whenElem condition f = if condition then f unit else HH.text ""

-- Unwrap some html from a maybe
maybeElement :: forall p i a. Maybe a -> (a -> HTML p i) -> HTML p i
maybeElement = flip $ maybe $ HH.text ""

-- | Used so we can run an action at a certain interval
intervalEventSource :: forall m. MonadAff m => Milliseconds -> ES.EventSource m Unit
intervalEventSource time =
  ES.affEventSource \emitter -> do
    fiber <-
      forkAff
        $ forever do
            liftAff $ delay time
            ES.emit emitter unit
    pure (ES.Finalizer (killFiber (error "Event source closed") fiber))
