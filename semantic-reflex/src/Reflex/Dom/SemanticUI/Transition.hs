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
  , Animation (..)
  , AnimationConfig (..)
  , Direction (..)
  , ActiveElConfig (..)
  , elWithAnim
  , elWithAnim'
  , HasAnimation (..)
  , elConfigAnimation
  , HasTransition (..)
  , elConfigTransition
  , HasAttributes (..)
  , elConfigAttributes
  , HasStyle (..)
  , elConfigStyle
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
  = Scale
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

directionToHidden :: Direction -> Bool
directionToHidden In = False
directionToHidden Out = True

class HasAddClass a where
  addClass :: a -> a

data TransitionConfig = TransitionConfig
  { _duration :: NominalDiffTime
  , _direction :: Maybe Direction
  }

instance Default TransitionConfig where
  def = TransitionConfig
    { _duration = 0.75
    , _direction = Nothing
    }

data Transition = Transition
  { _transitionType :: TransitionType
  , _config :: TransitionConfig
  }

getTransDuration :: Transition -> NominalDiffTime
getTransDuration (Transition _ TransitionConfig{..}) = _duration

getDirection :: Transition -> Maybe Direction
getDirection (Transition _ TransitionConfig{..}) = _direction

flipDirection :: Direction -> Direction
flipDirection In = Out
flipDirection Out = In

getTions :: Transition -> (NominalDiffTime, Maybe Direction)
getTions (Transition _ TransitionConfig{..}) = (_duration, _direction)

data Trans
  = TStart TransitionType NominalDiffTime Direction
  | TEnd Direction

getTime :: Transition -> Int
getTime (Transition _ TransitionConfig {..}) = fromEnum _duration

runTransition :: MonadWidget t m => Event t Transition
             -> m (AnimationAttrs t)
runTransition request = do

  let flipDir Nothing In = Out
      flipDir Nothing Out = In
      flipDir (Just d) _ = d

  lastDir <- foldDyn flipDir In $ leftmost [ getDirection <$> request ]

--  let printTime = formatTime defaultTimeLocale "%M:%S%Q"

  -- We transform the request into a more useful type, tagging with the
  -- direction. Must be promptly tagged or we use the wrong direction.
  reqTime <- performEvent $ ffor (attachPromptlyDyn lastDir request) $
    \(dir, Transition t (TransitionConfig dur _)) -> do
    timeRequested <- liftIO getCurrentTime
    return (timeRequested, TStart t dur dir)

--  performEvent_ $ ffor reqTime $ \(TStart t t' d d') -> do
--    liftJSM $ consoleLog $ "Got request " ++ show t ++ " at " ++ printTime t' ++ " for " ++ show d ++ " with direction " ++ show d'

  -- Holds the time that animations are currently running until. Future
  -- animation requests must be delayed until after the resultant time. We add
  -- 0.1s to the time to prevent out of order events (lazy solution).
  let timer (when, TStart _ howLong _) after
        | after < when = addUTCTime (howLong + 0.1) when
        | otherwise = addUTCTime (howLong + 0.1) after
  now <- liftIO getCurrentTime
  scheduleAfter <- foldDyn timer now reqTime

--  dyn $ ffor scheduleAfter $ liftJSM . consoleLog . printTime

  -- Queue the requests using the last value of scheduleAfter
  queue <- performEventAsync $ ffor (attach (current scheduleAfter) reqTime) $
    \(after, (_, t)) cb -> liftIO $ void $ forkIO $ do
      now <- getCurrentTime
      Concurrent.delay $ ceiling $ 1000000 * after `diffUTCTime` now
      cb t

--  performEvent_ $ ffor queue $ \(TStart t t' d d') -> do
--    liftJSM $ consoleLog $ "Queued request " ++ show t ++ " at " ++ printTime t' ++ " for " ++ show d ++ " with direction " ++ show d'

  -- Delay the queue events by their duration, to signal when the transitions
  -- have ended
  finish <- performEventAsync $ ffor queue $ \(TStart _ howLong d) cb ->
    liftIO $ void $ forkIO $ do
      Concurrent.delay $ ceiling $ howLong * 1000000
      cb $ TEnd d

  mClasses <- holdDyn Nothing $ fmap mkTransClasses $ leftmost [queue, finish]
  mStyle <- holdDyn Nothing $ fmap mkTransStyle $ leftmost [queue, finish]

  return $ AnimationAttrs mClasses mStyle

mkTransClasses :: Trans -> Maybe Classes
mkTransClasses (TStart t _ d)
  = Just $ Classes ["animating", "transition", toClassText t, toClassText d]
mkTransClasses (TEnd Out)
  = Just $ Classes ["transition", "hidden"]
mkTransClasses (TEnd In) = Nothing

mkTransStyle :: Trans -> Maybe Style
mkTransStyle (TStart _ d _) = Just $ Style $ "animation-duration" =: tshow d
mkTransStyle _ = Nothing

-- Animation

data AnimationConfig = AnimationConfig
  { _duration :: NominalDiffTime
  }

instance Default AnimationConfig where
  def = AnimationConfig
    { _duration = 0.75
    }

data Animation = Animation
  { _animationType :: AnimationType
  , _config :: AnimationConfig
  }

getDuration :: Animation -> NominalDiffTime
getDuration (Animation _ AnimationConfig{..}) = _duration

data AnimationAttrs t = AnimationAttrs
  { _class :: Dynamic t (Maybe Classes)
  , _style :: Dynamic t (Maybe Style)
  }

runAnimation :: MonadWidget t m => Event t Animation
             -> m (AnimationAttrs t)
runAnimation request = do
  rec finish <- delaySelf $ gate (isNothing <$> current mAnim)
                          $ fmap getDuration request
      mAnim <- holdDyn Nothing $ leftmost [Just <$> request, Nothing <$ finish]
  return $ AnimationAttrs (fmap animClasses <$> mAnim)
                          (fmap animStyle <$> mAnim)

animClasses :: Animation -> Classes
animClasses (Animation animType _)
  = Classes ["animating", "transition", toClassText animType]

animStyle :: Animation -> Style
animStyle (Animation _ AnimationConfig {..})
  = Style $ "animation-duration" =: tshow _duration

-- Active elements

data ActiveElConfig t = ActiveElConfig
  { _classes :: Active t Classes
  , _style :: Active t Style
  , _attrs :: Active t (Map Text Text)
  , _animation :: Maybe (Event t Animation)
  , _transition :: Maybe (Event t Transition)
  }

instance Default (ActiveElConfig t) where
  def = ActiveElConfig
    { _classes = Static mempty
    , _style = Static mempty
    , _attrs = Static mempty
    , _animation = Nothing
    , _transition = Nothing
    }

-- | Left biased
instance Reflex t => Semigroup (ActiveElConfig t) where
  ActiveElConfig a b c d e <> ActiveElConfig a' b' c' d' e'
    = ActiveElConfig (a <> a') (b <> b') (c <> c') d e

instance Reflex t => Monoid (ActiveElConfig t) where
  mempty = def
  mappend = (<>)

elWithAnim :: MonadWidget t m => Text -> ActiveElConfig t -> m a -> m a
elWithAnim elType conf = fmap snd . elWithAnim' elType conf

elWithAnim' :: MonadWidget t m => Text -> ActiveElConfig t -> m a
            -> m (Element EventResult (DomBuilderSpace m) t, a)
elWithAnim' _element ActiveElConfig {..} child = do
  --animAttrs <- traverse runAnimation _animation
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

class HasAnimation t a where
  animation :: Lens' a (Maybe (Event t Animation))

elConfigAnimation :: Lens' (ActiveElConfig t) (Maybe (Event t Animation))
elConfigAnimation f (ActiveElConfig c s at a t)
  = fmap (\a' -> ActiveElConfig c s at a' t) $ f a

class HasTransition t a where
  transition :: Lens' a (Maybe (Event t Transition))

elConfigTransition :: Lens' (ActiveElConfig t) (Maybe (Event t Transition))
elConfigTransition f (ActiveElConfig c s at a t)
  = fmap (\t' -> ActiveElConfig c s at a t') $ f t

class HasAttributes t a where
  attributes :: Lens' a (Active t (Map Text Text))

elConfigAttributes :: Lens' (ActiveElConfig t) (Active t (Map Text Text))
elConfigAttributes f (ActiveElConfig c s at a t)
  = fmap (\at' -> ActiveElConfig c s at' a t) $ f at

class HasStyle t a where
  style :: Lens' a (Active t Style)

elConfigStyle :: Lens' (ActiveElConfig t) (Active t Style)
elConfigStyle f (ActiveElConfig c s at a t)
  = fmap (\s' -> ActiveElConfig c s' at a t) $ f s

