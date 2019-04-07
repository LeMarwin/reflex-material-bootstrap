module Web.Reflex.Bootstrap.Router(
    Route(..)
  , route
  ) where

import Reflex
import Reflex.Dom

-- | Like 'Fix' for FRP widget, allow to endlessly jump into
-- returned routes of widgets
newtype Route t m a = Route { unRoute :: Event t (m (a, Route t m a)) }

instance (Reflex t, Functor m) => Functor (Route t m) where
  fmap f (Route e) = Route $ fmap (\(a, r) -> (f a, f <$> r)) <$> e

instance Reflex t => Semigroup (Route t m a) where
  (Route e1) <> (Route e2) = Route $ leftmost [e1, e2]

instance Reflex t => Monoid (Route t m a) where
  mempty = Route never

-- | Run widget that can replace itself with new widget constructed
-- internally in the original widget.
route :: forall t m a . MonadWidget t m => m (a, Route t m a) -> m (Dynamic t a)
route w = do
  rec (rd :: Dynamic t (a, Route t m a)) <- widgetHold w re
      let rd'   :: Dynamic t (Event t (m (a, Route t m a))) = unRoute . snd <$> rd
          re    :: Event t (m (a, Route t m a)) = switchPromptlyDyn rd'
  pure $ fst <$> rd

-- | Allows to switch over dynamic route to a single route
switchRoute :: forall t m a . MonadWidget t m => Dynamic t (Route t m a) -> Route t m a
switchRoute = Route . switch . current . fmap unRoute
