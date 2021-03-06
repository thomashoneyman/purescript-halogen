module Halogen.Query.Event where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe)
import Halogen.Emitter as Emitter
import Web.Event.Event as E
import Web.Event.EventTarget as ET

-- | Constructs an `Emitter` for a DOM event. Accepts a function that maps event
-- | values to a `Maybe`-wrapped action, allowing it to filter events if
-- | necessary.
eventListenerEventSource
  :: forall a
   . E.EventType
  -> ET.EventTarget
  -> (E.Event -> Maybe a)
  -> Emitter.Emitter a
eventListenerEventSource eventType target f =
  Emitter.makeEmitter \push -> do
    listener <- ET.eventListener \ev -> traverse_ push (f ev)
    ET.addEventListener eventType listener false target
    pure do
      ET.removeEventListener eventType listener false target
