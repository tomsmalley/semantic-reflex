{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Reflex.Active
  ( ActiveType
  , StaticType
  , DynamicType
  , TaggedActive (..)
  , taggedActive

  , taggedActiveListWithKey
  , taggedActiveSelectViewListWithKey

  , SingActive (..)
  , SActive (..)

  , Active (..)
  , distributeListOverActiveWith
  , unzipActive

  ) where

import Control.Monad.Fix (MonadFix)
import Data.Default
import Data.Map (Map)
import Data.Semigroup (Semigroup(..))
import Data.String (IsString(..))

import qualified Data.Map as M

import Reflex
import Reflex.Dom.Core

-- | Kind promoted tag with types 'StaticType' and 'DynamicType'.
data ActiveType where
  StaticType :: ActiveType
  DynamicType :: ActiveType

type StaticType = 'StaticType
type DynamicType = 'DynamicType

data SActive (a :: ActiveType) where
  SStatic :: SActive StaticType
  SDynamic :: SActive DynamicType

class SingActive a where sing :: SActive a
instance SingActive 'StaticType where sing = SStatic
instance SingActive 'DynamicType where sing = SDynamic

-- | Similar to 'Either a (Dynamic t a)' but statically determined
data TaggedActive (active :: ActiveType) t a where
  TaggedStatic :: a -> TaggedActive StaticType t a
  TaggedDynamic :: Dynamic t a -> TaggedActive DynamicType t a

instance Reflex t => Functor (TaggedActive active t) where
  fmap f (TaggedStatic a) = TaggedStatic $ f a
  fmap f (TaggedDynamic d) = TaggedDynamic $ f <$> d

instance (SingActive active, Reflex t)
  => Applicative (TaggedActive active t) where
  pure = case sing :: SActive active of
    SStatic -> TaggedStatic
    SDynamic -> TaggedDynamic . pure
  TaggedStatic f <*> TaggedStatic a = TaggedStatic $ f a
  TaggedDynamic f <*> TaggedDynamic a = TaggedDynamic $ f <*> a

instance (Reflex t, Semigroup a) => Semigroup (TaggedActive active t a) where
  TaggedStatic a <> TaggedStatic b  = TaggedStatic $ a <> b
  TaggedDynamic a <> TaggedDynamic b = TaggedDynamic $ zipDynWith (<>) a b

instance (SingActive active, Reflex t, IsString a)
  => IsString (TaggedActive active t a) where
    fromString = pure . fromString

-- | Case analysis for 'Active'. If the value is 'Static a', apply the first
-- function to 'a'; if it is 'Dynamic d', apply the second function to the
-- 'Dynamic' 'd'.
taggedActive :: (a -> b) -> (Dynamic t a -> b) -> TaggedActive active t a -> b
taggedActive f g = \case
  TaggedStatic a -> f a
  TaggedDynamic a -> g a
{-# INLINABLE taggedActive #-}

-- | 'listWithKey' for 'Active'. Equivalent to 'M.traverseWithKey' for
-- 'Static', and 'listWithKey' for 'Dynamic'.
taggedActiveListWithKey
  :: (Ord k, Adjustable t m, PostBuild t m, MonadFix m, MonadHold t m)
  => TaggedActive active t (Map k v)
  -> (k -> TaggedActive active t v -> m a)
  -> m (TaggedActive active t (Map k a))
taggedActiveListWithKey a f = case a of
  TaggedStatic m -> TaggedStatic <$>
    M.traverseWithKey (\k -> f k . TaggedStatic) m
  TaggedDynamic m -> TaggedDynamic <$>
    listWithKey m (\k -> f k . TaggedDynamic)

-- | Create a dynamically-changing set of widgets, one of which is selected at
-- any time.
taggedActiveSelectViewListWithKey :: forall active t m k v a.
  (Adjustable t m, Ord k, PostBuild t m, MonadHold t m, MonadFix m)
  => Dynamic t k
  -> TaggedActive active t (Map k v)
  -> (k -> TaggedActive active t v -> Dynamic t Bool
        -> m (Element EventResult (DomBuilderSpace m) t, Event t a))
  -> m (TaggedActive active t (Map k (Element EventResult (DomBuilderSpace m) t)), Event t k)
taggedActiveSelectViewListWithKey selection vals mkChild = do
  let selectionDemux = demux selection
  selectChild <- taggedActiveListWithKey vals $ \k v -> do
    let selected = demuxed selectionDemux k
    (e, selectSelf) <- mkChild k v selected
    pure (e, k <$ selectSelf)
  pure
    ( fmap fst <$> selectChild
    , taggedActive id switchPromptlyDyn $
        leftmost . fmap snd . M.elems <$> selectChild
    )


data Active t a
  = Static a
  | Dyn (Dynamic t a)

deriving instance Reflex t => Functor (Active t)

instance Reflex t => Applicative (Active t) where
  pure = Static
  Static f <*> Static a = Static (f a)
  Static f <*> Dyn a = Dyn (pure f <*> a)
  Dyn f <*> Static a = Dyn (f <*> pure a)
  Dyn f <*> Dyn a = Dyn (f <*> a)

instance (Reflex t, Monoid a) => Monoid (Active t a) where
  mempty = Static mempty
  Static a `mappend` Static b = Static (a `mappend` b)
  Static a `mappend` Dyn b = Dyn (pure a `mappend` b)
  Dyn a `mappend` Static b = Dyn (a `mappend` pure b)
  Dyn a `mappend` Dyn b = Dyn (a `mappend` b)
  mconcat = distributeListOverActiveWith mappend mconcat

instance IsString a => IsString (Active t a) where
  fromString = Static . fromString

instance Default a => Default (Active t a) where
  def = Static def

unzipActive :: [Active t a] -> ([a], [Dynamic t a])
unzipActive = foldr f ([], []) where
  f (Static a) (as, dAs) = (a : as, dAs)
  f (Dyn dA) (as, dAs) = (as, dA : dAs)

-- | Usually this function is called on a list with many static items and a few
-- dynamic items. Hence we optimise this case - for cases where all items are
-- static, it is much faster. For cases where only a few items are dynamic, it
-- can be ~2x faster.
distributeListOverActiveWith
  :: Reflex t
  => (b -> b -> b) -> ([a] -> b) -> [Active t a] -> Active t b
distributeListOverActiveWith joinB f as
  = if null dynamics
    then Static $ f consts
    else Dyn $ zipDynWith joinB
      (pure $ f consts)
      (distributeListOverDynWith f dynamics)
  where (consts, dynamics) = unzipActive as

