module Halogen.Emitter where

import Prelude

import Data.Array (deleteBy)
import Data.Foldable (traverse_)
import Data.Functor.Contravariant (class Contravariant)
import Effect (Effect)
import Effect.Ref as Ref
import Safe.Coerce (coerce)
import Unsafe.Reference (unsafeRefEq)

newtype Listener a = Listener (a -> Effect Unit)

instance contravariantListener :: Contravariant Listener where
  cmap f (Listener g) = Listener (g <<< f)

notify :: forall a. a -> Listener a -> Effect Unit
notify a (Listener f) = f a

newtype Subscription = Subscription (Effect Unit)

derive newtype instance semigroupSubscription :: Semigroup Subscription
derive newtype instance monoidSubscription :: Monoid Subscription

unsubscribe :: Subscription -> Effect Unit
unsubscribe (Subscription unsub) = unsub

newtype Emitter a = Emitter ((a -> Effect Unit) -> Effect Subscription)

instance functorEmitter :: Functor Emitter where
  map f (Emitter e) = Emitter \k -> e (k <<< f)

type EmitIO a =
  { emitter :: Emitter a
  , listener :: Listener a
  }

create :: forall a. Effect (EmitIO a)
create = do
  subscribers <- Ref.new []
  pure
    { emitter: Emitter \k -> do
        _ <- Ref.modify (_ <> [k]) subscribers
        pure $ Subscription do
          _ <- Ref.modify (deleteBy unsafeRefEq k) subscribers
          pure unit
    , listener: Listener \a -> do
        Ref.read subscribers >>= traverse_ \k -> k a
    }

subscribe
  :: forall r a
   . Emitter a
  -> (a -> Effect r)
  -> Effect Subscription
subscribe (Emitter e) k = e (void <<< k)

makeEmitter
  :: forall a
   . ((a -> Effect Unit) -> Effect (Effect Unit))
  -> Emitter a
makeEmitter = coerce
