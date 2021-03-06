module Halogen.Emitter where

import Prelude

import Data.Array (deleteBy)
import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Ref as Ref
import Unsafe.Reference (unsafeRefEq)

-- event type
newtype Emitter a = Emitter ((a -> Effect Unit) -> Effect (Effect Unit))

instance functorEmitter :: Functor Emitter where
  map f (Emitter e) = Emitter \k -> e (k <<< f)

type EmitIO a =
  { emitter :: Emitter a
  , push :: a -> Effect Unit
  }

create :: forall a. Effect (EmitIO a)
create = do
  subscribers <- Ref.new []
  pure
    { emitter: Emitter \k -> do
        _ <- Ref.modify (_ <> [k]) subscribers
        pure do
          _ <- Ref.modify (deleteBy unsafeRefEq k) subscribers
          pure unit
    , push: \a -> do
        Ref.read subscribers >>= traverse_ \k -> k a
    }

subscribe
  :: forall r a
   . Emitter a
  -> (a -> Effect r)
  -> Effect (Effect Unit)
subscribe (Emitter e) k = e (void <<< k)

makeEmitter
  :: forall a
   . ((a -> Effect Unit) -> Effect (Effect Unit))
  -> Emitter a
makeEmitter = Emitter
