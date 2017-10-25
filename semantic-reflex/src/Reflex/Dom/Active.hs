{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}

module Reflex.Dom.Active where

import Data.Map (Map)
import Data.Text (Text)
import Data.Semigroup
import Data.String

import Reflex
import Reflex.Dom.Core (DomBuilder(..), Element, EventResult)
import qualified Reflex.Dom.Core as Dom

data Active t a
  = Static a
  | Dynamic (Dynamic t a)

elActiveAttr'
  :: (PostBuild t m, DomBuilder t m)
  => Text -> Active t (Map Text Text) -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
elActiveAttr' e = \case
  Static attrs -> Dom.elAttr' e attrs
  Dynamic attrs -> Dom.elDynAttr' e attrs

elActiveAttr
  :: (PostBuild t m, DomBuilder t m)
  => Text -> Active t (Map Text Text) -> m a -> m a
elActiveAttr e a = fmap snd . elActiveAttr' e a

activeToDyn :: Reflex t => Active t a -> Dynamic t a
activeToDyn (Static a) = pure a
activeToDyn (Dynamic a) = a

tagActive :: Reflex t => Active t a -> Event t b -> Event t a
tagActive (Static a) evt = a <$ evt
tagActive (Dynamic d) evt = tag (current d) evt

active :: (a -> b) -> (Dynamic t a -> b) -> Active t a -> b
active f _ (Static a) = f a
active _ f (Dynamic a) = f a

instance Reflex t => Functor (Active t) where
  fmap f (Static a)  = Static  $ f a
  fmap f (Dynamic a) = Dynamic $ fmap f a

instance Reflex t => Applicative (Active t) where
  pure = Static
  Static f  <*> Static a  = Static  $ f a
  Static f  <*> Dynamic a = Dynamic $ f <$> a
  Dynamic f <*> Static a  = Dynamic $ fmap ($ a) f
  Dynamic f <*> Dynamic a = Dynamic $ f <*> a

instance IsString a => IsString (Active t a) where
  fromString = Static . fromString

instance (Reflex t, Semigroup a) => Semigroup (Active t a) where
  Static a  <> Static b  = Static  $ a <> b
  Static a  <> Dynamic b = Dynamic $ fmap (a <>) b
  Dynamic a <> Static b  = Dynamic $ fmap (<> b) a
  Dynamic a <> Dynamic b = Dynamic $ zipDynWith (<>) a b

-- TODO: efficient mconcat
instance (Reflex t, Monoid a, Semigroup a) => Monoid (Active t a) where
  mempty = Static mempty
  mappend = (<>)
