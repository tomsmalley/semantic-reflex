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
  , TransitionType (..)
  , TransitionConfig (..)
  , AnimationType (..)
  , AnimationConfig (..)
  , Direction (..)
  , ActiveElConfig (..)
  , elWithAnim
  , elWithAnim'
  , HasTransition (..)
  , elConfigTransition
  , HasAttributes (..)
  , elConfigAttributes
  , HasStyle (..)
  , elConfigStyle
  , HasClasses (..)
  , elConfigClasses
  ) where

import Control.Concurrent
import qualified Control.Concurrent.Thread.Delay as Concurrent
import Control.Lens ((%~), Lens')
import Control.Monad ((<=<), void)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Default (Default(..))
import Data.Functor.Misc (WrapArg(..))
import Data.Maybe (isJust, isNothing)
import Data.Proxy (Proxy(..))
import Data.Semigroup
import qualified Data.Set as S
import Data.Map (Map)
import Data.Text (Text)
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.HTMLInputElement as Input
import Language.Javascript.JSaddle (liftJSM)
import Reflex
import Reflex.Dom.Core hiding (Drop, HasAttributes)

import Data.Time

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

flipDirection :: Direction -> Direction
flipDirection In = Out
flipDirection Out = In

class HasAddClass a where
  addClass :: a -> a

data TransitionConfig = TransitionConfig
  { _duration :: NominalDiffTime
  , _direction :: Maybe Direction
  , _forceVisible :: Bool
  }

instance Default TransitionConfig where
  def = TransitionConfig
    { _duration = 0.75
    , _direction = Nothing
    , _forceVisible = False
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

runTransition :: MonadWidget t m => Event t Transition -> m (AnimationAttrs t)
runTransition transitionRequest = do

  let flipDir Nothing In = Out
      flipDir Nothing Out = In
      flipDir (Just d) _ = d

  lastDir <- foldDyn flipDir In $ leftmost [ getDirection <$> transitionRequest ]

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
  now <- liftIO getCurrentTime
  scheduleAfter <- foldDyn timer now reqTime

  -- Queue the requests using the last value of scheduleAfter
  queue <- performEventAsync $ ffor (attach (current scheduleAfter) reqTime) $
    \(after, (_, d, t)) cb -> liftIO $ void $ forkIO $ do
      now <- getCurrentTime
      Concurrent.delay $ ceiling $ 1000000 * after `diffUTCTime` now
      cb (d, t)

  -- Delay the queue events by their duration, to signal when the transitions
  -- have ended
  finish <- performEventAsync $ ffor queue $ \(d, t) cb -> case t of
    Transition Instant TransitionConfig{..} -> liftIO $ cb (d, _forceVisible)
    Transition _ TransitionConfig{..} -> liftIO $ void $ forkIO $ do
      Concurrent.delay $ ceiling $ _duration * 1000000
      cb (d, _forceVisible)
    Animation _ AnimationConfig{..} -> liftIO $ void $ forkIO $ do
      Concurrent.delay $ ceiling $ _duration * 1000000
      cb (In, False)

  mClasses <- holdDyn Nothing $ leftmost
    [ mkTransClasses <$> queue
    , mkTransEndClasses <$> finish ]
  mStyle <- holdDyn Nothing $ leftmost
    [ mkTransStyle . snd <$> queue
    , Nothing <$ finish ]
  return $ AnimationAttrs mClasses mStyle

mkTransClasses :: (Direction, Transition) -> Maybe Classes
mkTransClasses (d, Transition t _)
  = Just $ Classes ["animating", "transition", toClassText t, toClassText d]
mkTransClasses (_, Animation t _)
  = Just $ Classes ["animating", "transition", toClassText t]

mkTransEndClasses :: (Direction, Bool) -> Maybe Classes
mkTransEndClasses (Out, _) = Just $ Classes ["transition", "hidden"]
mkTransEndClasses (In, True) = Just $ Classes ["transition", "visible"]
mkTransEndClasses (In, False) = Nothing

mkTransStyle :: Transition -> Maybe Style
mkTransStyle t = Just $ Style $ "animation-duration" =: tshow (getDuration t)

-- Active elements

data ActiveElConfig t = ActiveElConfig
  { _classes :: Active t Classes
  , _style :: Active t Style
  , _attrs :: Active t (Map Text Text)
  , _transition :: Maybe (Event t Transition)
  }

instance Default (ActiveElConfig t) where
  def = ActiveElConfig
    { _classes = Static mempty
    , _style = Static mempty
    , _attrs = Static mempty
    , _transition = Nothing
    }

-- | Left biased
instance Reflex t => Semigroup (ActiveElConfig t) where
  ActiveElConfig a b c d <> ActiveElConfig a' b' c' d'
    = ActiveElConfig (a <> a') (b <> b') (c <> c') d

instance Reflex t => Monoid (ActiveElConfig t) where
  mempty = def
  mappend = (<>)

elWithAnim :: MonadWidget t m => Text -> ActiveElConfig t -> m a -> m a
elWithAnim elType conf = fmap snd . elWithAnim' elType conf

elWithAnim' :: MonadWidget t m => Text -> ActiveElConfig t -> m a
            -> m (Element EventResult (DomBuilderSpace m) t, a)
elWithAnim' _element ActiveElConfig {..} child = do
  transAttrs <- traverse runTransition _transition
  case transAttrs of
    Nothing -> let activeAttrs = mkAttrs <$> _classes <*> _style <*> _attrs
                   mkAttrs classes style attrs
                    = classAttr classes <> styleAttr style <> attrs
                in elActiveAttr' _element activeAttrs child
    Just (AnimationAttrs mClasses mStyle)  -> do
      let activeAttrs = mkAttrs <$> _classes <*> Dynamic mClasses
                                <*> _style <*> Dynamic mStyle
                                <*> _attrs
          mkAttrs classes mClasses style mStyle attrs
            = classAttr (maybe classes (<> classes) mClasses)
             <> styleAttr (maybe style (<> style) mStyle)
             <> attrs
      elActiveAttr' _element activeAttrs child

-- | Delay an Event's occurrences by a given amount in seconds.
delaySelf :: (PerformEvent t m, TriggerEvent t m, MonadIO (Performable m))
      => Event t NominalDiffTime -> m (Event t NominalDiffTime)
delaySelf e = performEventAsync $ ffor e $ \dt cb ->
  liftIO $ void $ forkIO $ do
    Concurrent.delay $ ceiling $ dt * 1000000
    cb dt

delaySnd :: (PerformEvent t m, TriggerEvent t m, MonadIO (Performable m))
         => Event t (a, NominalDiffTime) -> m (Event t a)
delaySnd e = performEventAsync $ ffor e $ \(a, dt) cb ->
  liftIO $ void $ forkIO $ do
    Concurrent.delay $ ceiling $ dt * 1000000
    cb a


-- Lenses

class HasTransition t a where
  transition :: Lens' a (Maybe (Event t Transition))

elConfigTransition :: Lens' (ActiveElConfig t) (Maybe (Event t Transition))
elConfigTransition f (ActiveElConfig c s at t)
  = fmap (\t' -> ActiveElConfig c s at t') $ f t

class HasAttributes t a where
  attributes :: Lens' a (Active t (Map Text Text))

elConfigAttributes :: Lens' (ActiveElConfig t) (Active t (Map Text Text))
elConfigAttributes f (ActiveElConfig c s at t)
  = fmap (\at' -> ActiveElConfig c s at' t) $ f at

class HasStyle t a where
  style :: Lens' a (Active t Style)

elConfigStyle :: Lens' (ActiveElConfig t) (Active t Style)
elConfigStyle f (ActiveElConfig c s at t)
  = fmap (\s' -> ActiveElConfig c s' at t) $ f s

class HasClasses t a where
  classes :: Lens' a (Active t Classes)

elConfigClasses :: Lens' (ActiveElConfig t) (Active t Classes)
elConfigClasses f (ActiveElConfig c s at t)
  = fmap (\c' -> ActiveElConfig c' s at t) $ f c

