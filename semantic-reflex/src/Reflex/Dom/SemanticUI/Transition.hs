{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedLists        #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE RecursiveDo            #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE InstanceSigs           #-}

-- | Semantic UI transitions. Pure reflex implementation is provided.
-- https://semantic-ui.com/modules/transition.html
module Reflex.Dom.SemanticUI.Transition
  (
  -- * Transition
    Transition (..)
  , TransConfig (..)
  , TransitionType (..)
  , TransitionConfig (..)
  , AnimationType (..)
  , AnimationConfig (..)
  , Direction (..)
  , ActiveElConfig (..)
  , elWithAnim
  , elWithAnim'
  , elConfigTransition
  , elConfigAttributes
  , elConfigStyle
  , elConfigClasses
  , divClass
  , SetValue' (..)
  ) where

import Control.Concurrent
import qualified Control.Concurrent.Thread.Delay as Concurrent
import Control.Lens (Lens')
import Control.Monad (void)
import Control.Monad.Trans (liftIO, lift)
import Data.Default (Default(..))
import Data.Semigroup
import Data.Map (Map)
import Data.Text (Text)
import Reflex
import Reflex.Dom.Core hiding (Drop, HasAttributes, divClass, elAttr')

import Data.Time

import Reflex.Dom.Active
import Reflex.Dom.SemanticUI.Common

data TransitionType
  = Instant
  | Scale
  | Fade | FadeUp | FadeDown | FadeLeft | FadeRight
  | HorizontalFlip | VerticalFlip
  | Drop
  | FlyLeft | FlyRight | FlyUp | FlyDown
  | SwingLeft | SwingRight | SwingUp | SwingDown
  | Browse | BrowseRight
  | SlideDown | SlideUp | SlideLeft | SlideRight
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance Default TransitionType where
  def = Fade

instance ToClassText TransitionType where
  toClassText Instant = ""
  toClassText Scale = "scale"
  toClassText Fade = "fade"
  toClassText FadeUp = "fade up"
  toClassText FadeDown = "fade down"
  toClassText FadeLeft = "fade left"
  toClassText FadeRight = "fade right"
  toClassText HorizontalFlip = "horizontal flip"
  toClassText VerticalFlip = "vertical flip"
  toClassText Drop = "drop"
  toClassText FlyLeft = "fly left"
  toClassText FlyRight = "fly right"
  toClassText FlyUp = "fly up"
  toClassText FlyDown = "fly down"
  toClassText SwingLeft = "swing left"
  toClassText SwingRight = "swing right"
  toClassText SwingUp = "swing up"
  toClassText SwingDown = "swing down"
  toClassText Browse = "browse"
  toClassText BrowseRight = "browse right"
  toClassText SlideDown = "slide down"
  toClassText SlideUp = "slide up"
  toClassText SlideLeft = "slide left"
  toClassText SlideRight = "slide right"

-- Loop while (Dynamic t Bool)
-- Chain animations
-- transition on a hlist of transitionables

data AnimationType = Jiggle | Flash | Shake | Pulse | Tada | Bounce
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToClassText AnimationType where
  toClassText Jiggle = "jiggle"
  toClassText Flash = "flash"
  toClassText Shake = "shake"
  toClassText Pulse = "pulse"
  toClassText Tada = "tada"
  toClassText Bounce = "bounce"

data Direction = In | Out
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToClassText Direction where
  toClassText In = "in"
  toClassText Out = "out"

data TransitionConfig = TransitionConfig
  { _duration :: NominalDiffTime
  , _direction :: Maybe Direction
  }

instance Default TransitionConfig where
  def = TransitionConfig
    { _duration = 0.75
    , _direction = Nothing
    }

data AnimationConfig = AnimationConfig
  { _duration :: NominalDiffTime
  }

instance Default AnimationConfig where
  def = AnimationConfig
    { _duration = 0.75
    }

data Transition
  = Transition TransitionType TransitionConfig
  | Animation AnimationType AnimationConfig

getDuration :: Transition -> NominalDiffTime
getDuration (Transition Instant _) = 0
getDuration (Transition _ TransitionConfig{..}) = _duration
getDuration (Animation _ AnimationConfig{..}) = _duration

getDirection :: Transition -> Maybe Direction
getDirection (Transition _ TransitionConfig{..}) = _direction
getDirection (Animation _ _) = Just In

data AnimationAttrs t = AnimationAttrs
  { _class :: Dynamic t (Maybe Classes)
  , _style :: Dynamic t (Maybe Style)
  }

runTransition
  :: MonadWidget t m
  => TransConfig t
--  => Bool                 -- ^ Element is hidden initially?
--  -> Event t Transition   -- ^ Transition events
  -> m (AnimationAttrs t)
runTransition (TransConfig transitionRequest initHidden forceVisible extraClasses) = do

  let flipDir Nothing In = Out
      flipDir Nothing Out = In
      flipDir (Just d) _ = d

  let initDirection = if initHidden then Out else In

  lastDir <- foldDyn flipDir initDirection $ getDirection <$> transitionRequest

  -- We transform the transitionRequest into a more useful type, tagging with
  -- the direction. Must be promptly tagged or we use the wrong direction.
  reqTime <- performEvent $ ffor (attachPromptlyDyn lastDir transitionRequest) $
    \(dir, t) -> do
      timeRequested <- liftIO getCurrentTime
      return (timeRequested, dir, t)

  -- Holds the time that animations are currently running until. Future
  -- animation requests must be delayed until after the resultant time. We add
  -- 0.1s to the time to prevent out of order events (lazy solution).
  let timer (when, _, t) after
        | after < when = addUTCTime (getDuration t + 0.1) when
        | otherwise = addUTCTime (getDuration t + 0.1) after
  scheduleAfter <- do
    currentTime <- liftIO getCurrentTime
    foldDyn timer currentTime reqTime

  -- Queue the requests using the last value of scheduleAfter
  queue <- performEventAsync $ ffor (attach (current scheduleAfter) reqTime) $
    \(after, (_, d, t)) cb -> liftIO $ void $ forkIO $ do
      currentTime <- getCurrentTime
      Concurrent.delay $ ceiling $ 1000000 * after `diffUTCTime` currentTime
      cb (d, t)

  -- Delay the queue events by their duration, to signal when the transitions
  -- have ended
  finish <- performEventAsync $ ffor queue $ \(d, t) cb -> case t of
    Transition Instant TransitionConfig{..} -> liftIO $ cb d
    Transition _ TransitionConfig{..} -> liftIO $ void $ forkIO $ do
      Concurrent.delay $ ceiling $ _duration * 1000000
      cb d
    Animation _ AnimationConfig{..} -> liftIO $ void $ forkIO $ do
      Concurrent.delay $ ceiling $ _duration * 1000000
      cb In

  mClasses <- holdDyn (mkTransEndClasses extraClasses forceVisible initDirection) $ leftmost
    [ mkTransClasses <$> queue
    , mkTransEndClasses extraClasses forceVisible <$> finish ]
  mStyle <- holdDyn Nothing $ leftmost
    [ mkTransStyle . snd <$> queue
    , Nothing <$ finish ]
  return $ AnimationAttrs mClasses mStyle

mkTransClasses :: (Direction, Transition) -> Maybe Classes
mkTransClasses (d, Transition t _)
  = Just $ Classes ["animating", "transition", toClassText t, toClassText d]
mkTransClasses (_, Animation t _)
  = Just $ Classes ["animating", "transition", toClassText t]

mkTransEndClasses :: (Bool -> Maybe Classes) -> Bool -> Direction
                  -> Maybe Classes
mkTransEndClasses f _ Out = f False <> (Just $ Classes ["transition", "hidden"])
mkTransEndClasses f True In = f True <> (Just $ Classes ["transition", "visible"])
mkTransEndClasses f False In = f True

mkTransStyle :: Transition -> Maybe Style
mkTransStyle t = Just $ Style $ "animation-duration" =: tshow (getDuration t)

-- Active elements

data ActiveElConfig t = ActiveElConfig
  { _classes :: Active t Classes
  , _style :: Active t Style
  , _attrs :: Active t (Map Text Text)
  , _transition :: Maybe (TransConfig t)
  }

instance Reflex t => Default (ActiveElConfig t) where
  def = ActiveElConfig
    { _classes = pure mempty
    , _style = pure mempty
    , _attrs = pure mempty
    , _transition = Nothing
    }

data TransConfig t = TransConfig
  { _event :: Event t Transition
  , _initial :: Bool
  , _forceVisible :: Bool
  , _extraClasses :: Bool -> Maybe Classes
  }

instance Reflex t => Default (TransConfig t) where
  def = TransConfig
    { _event = never
    , _initial = False
    , _forceVisible = False
    , _extraClasses = const Nothing
    }

instance Reflex t => Semigroup (TransConfig t) where
  TransConfig e1 i v f1 <> TransConfig e2 _ _ f2 = TransConfig (leftmost [e1, e2]) i v (f1 <> f2)


data SetValue' t a b = SetValue
  { _initial :: a
  , _event :: Maybe (Event t b)
  }

instance Reflex t => Semigroup (SetValue' t a b) where
  SetValue a mEvt1 <> SetValue _ mEvt2 = SetValue a (mCombined mEvt1 mEvt2)
    where
      mCombined Nothing Nothing = Nothing
      mCombined (Just e1) (Just e2) = Just $ leftmost [e1, e2]
      mCombined (Just e1) Nothing = Just e1
      mCombined Nothing (Just e2) = Just e2

-- | Left biased
instance Reflex t => Semigroup (ActiveElConfig t) where
  ActiveElConfig a b c d <> ActiveElConfig a' b' c' d'
    = ActiveElConfig (a <> a') (b <> b') (c <> c') (d <> d')

instance Reflex t => Monoid (ActiveElConfig t) where
  mempty = def
  mappend = (<>)

elWithAnim :: MonadWidget t m => Text -> ActiveElConfig t -> Component r m a -> Component None m a
elWithAnim elType conf = fmap snd . elWithAnim' elType conf

elWithAnim'
  :: MonadWidget t m
  => Text
  -> ActiveElConfig t
  -> Component r m a
  -> Component None m (Element EventResult (DomBuilderSpace m) t, a)
elWithAnim' _element ActiveElConfig {..} (Component child) = Component $ do
  transAttrs <- traverse runTransition _transition
  case transAttrs of
    Nothing -> do
      let activeAttrs = mkAttrs <$> _classes <*> _style <*> _attrs
          mkAttrs c s attrs = classAttr c <> styleAttr s <> attrs
      elActiveAttr' _element activeAttrs child

    Just (AnimationAttrs mDynClasses mDynStyle) -> do

      let activeAttrs = mkAttrs <$> _classes <*> Dynamic mDynClasses
                              <*> _style <*> Dynamic mDynStyle
                              <*> _attrs

          mkAttrs c mClasses s mStyle attrs
            = classAttr (maybe c (<> c) mClasses)
            <> styleAttr (maybe s (<> s) mStyle)
            <> attrs

      elActiveAttr' _element activeAttrs child

divClass :: MonadWidget t m
         => Active t Classes -> Component None m a -> Component None m a
divClass c = elWithAnim "div" (def & elConfigClasses .~ c)

-- Lenses

elConfigTransition :: Lens' (ActiveElConfig t) (Maybe (TransConfig t))
elConfigTransition f (ActiveElConfig c s at t)
  = fmap (\t' -> ActiveElConfig c s at t') $ f t

elConfigAttributes :: Lens' (ActiveElConfig t) (Active t (Map Text Text))
elConfigAttributes f (ActiveElConfig c s at t)
  = fmap (\at' -> ActiveElConfig c s at' t) $ f at

elConfigStyle :: Lens' (ActiveElConfig t) (Active t Style)
elConfigStyle f (ActiveElConfig c s at t)
  = fmap (\s' -> ActiveElConfig c s' at t) $ f s

elConfigClasses :: Lens' (ActiveElConfig t) (Active t Classes)
elConfigClasses f (ActiveElConfig c s at t)
  = fmap (\c' -> ActiveElConfig c' s at t) $ f c

