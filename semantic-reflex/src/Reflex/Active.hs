{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Reflex.Active
  ( ActiveType
  , StaticType
  , DynamicType
  , Active (..)
  , active

  , activeListWithKey
  , activeSelectViewListWithKey

  , SingActive (..)
  , SActive (..)
  ) where

import Control.Monad.Fix (MonadFix)
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
data Active (active :: ActiveType) t a where
  Static :: a -> Active StaticType t a
  Dynamic :: Dynamic t a -> Active DynamicType t a

instance Reflex t => Functor (Active active t) where
  fmap f (Static a) = Static $ f a
  fmap f (Dynamic d) = Dynamic $ f <$> d

instance (SingActive active, Reflex t) => Applicative (Active active t) where
  pure = case sing :: SActive active of
    SStatic -> Static
    SDynamic -> Dynamic . pure
  Static f <*> Static a = Static $ f a
  Dynamic f <*> Dynamic a = Dynamic $ f <*> a

instance (Reflex t, Semigroup a) => Semigroup (Active active t a) where
  Static a <> Static b  = Static $ a <> b
  Dynamic a <> Dynamic b = Dynamic $ zipDynWith (<>) a b

instance (SingActive active, Reflex t, IsString a)
  => IsString (Active active t a) where
    fromString = pure . fromString

-- | Case analysis for 'Active'. If the value is 'Static a', apply the first
-- function to 'a'; if it is 'Dynamic d', apply the second function to the
-- 'Dynamic' 'd'.
active :: (a -> b) -> (Dynamic t a -> b) -> Active active t a -> b
active f g = \case
  Static a -> f a
  Dynamic a -> g a

-- | 'listWithKey' for 'Active'. Equivalent to 'M.traverseWithKey' for
-- 'Static', and 'listWithKey' for 'Dynamic'.
activeListWithKey
  :: (Ord k, Adjustable t m, PostBuild t m, MonadFix m, MonadHold t m)
  => Active active t (Map k v)
  -> (k -> Active active t v -> m a)
  -> m (Active active t (Map k a))
activeListWithKey a f = case a of
  Static m -> Static <$> M.traverseWithKey (\k -> f k . Static) m
  Dynamic m -> Dynamic <$> listWithKey m (\k -> f k . Dynamic)

-- | Create a dynamically-changing set of widgets, one of which is selected at
-- any time.
activeSelectViewListWithKey :: forall active t m k v a.
  (Adjustable t m, Ord k, PostBuild t m, MonadHold t m, MonadFix m)
  => Dynamic t k
  -> Active active t (Map k v)
  -> (k -> Active active t v -> Dynamic t Bool -> m (El t, Event t a))
  -> m (Active active t (Map k (El t)), Event t k)
activeSelectViewListWithKey selection vals mkChild = do
  let selectionDemux = demux selection
  selectChild <- activeListWithKey vals $ \k v -> do
    let selected = demuxed selectionDemux k
    (e, selectSelf) <- mkChild k v selected
    pure (e, k <$ selectSelf)
  pure
    ( fmap fst <$> selectChild
    , active id switchPromptlyDyn $
        leftmost . fmap snd . M.elems <$> selectChild
    )
