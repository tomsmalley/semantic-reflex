{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Semantic UI transitions. Pure reflex implementation is provided.
-- https://semantic-ui.com/modules/transition.html
module Reflex.Dom.SemanticUI.Transition
  (
  -- * Transition
    Transition (..)

  , Action (..)
  , actionEvent
  , actionInitialDirection
  , actionForceVisible

  , TransitionType (..)
  , AnimationType (..)

  , TransitionConfig (..)
  , transitionDuration
  , transitionDirection
  , transitionCancelling

  , Direction (..)
  , flipDirection

  , dynClasses
  , ActiveElConfig (..)
  , UI
  , ui
  , ui'
  , elConfigTransition
  , elConfigAttributes
  , elConfigStyle
  , elConfigClasses

  , HasAction (..)
  , HasAttributes (..)
  , HasStyle (..)
  , HasClasses (..)
  , HasElConfig (..)

  -- * SetValue
  , SetValue' (..)
  , SetValue
  , initial, event

  , AnimationAttrs(..)
  , runAction
  ) where

import Control.Applicative (Alternative(..))
import Control.Concurrent
import Control.Lens (Lens')
import Control.Lens.TH (makeLenses)
import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans (liftIO, MonadIO)

import Data.Align
import Data.Default (Default(..))
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Semigroup
import Data.These
import Data.Time
import Data.Text (Text)

import Reflex
import Reflex.Dom.Core hiding (Drop, HasAttributes, SetValue)

import qualified Control.Concurrent.Thread.Delay as Concurrent
import qualified Data.IntMap as IM

import Reflex.Active
import Reflex.Dom.SemanticUI.Common

-- | Transition types as listed in
-- https://semantic-ui.com/modules/transition.html#transitions
-- Include an extra type, 'Instant', for convenience.
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

-- | Animation types as listed in
-- https://semantic-ui.com/modules/transition.html#static-animations
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

-- | Returned by 'runAction', controls the relavant animation / transition
-- classes and styles.
data AnimationAttrs t = AnimationAttrs
  { _animationAttrsClass :: Active t (Maybe Classes)
  , _animationAttrsStyle :: Active t (Maybe Style)
  }

-- | Individual transition events
data Transition
  = Transition TransitionType TransitionConfig
  | Animation AnimationType TransitionConfig

getDirection :: Transition -> Maybe Direction
getDirection (Transition _ TransitionConfig{..}) = _transitionDirection
getDirection (Animation _ TransitionConfig{..}) = _transitionDirection

getDuration :: Transition -> NominalDiffTime
getDuration (Transition Instant _) = 0
getDuration (Transition _ TransitionConfig{..}) = _transitionDuration
getDuration (Animation _ TransitionConfig{..}) = _transitionDuration

isCancelling :: Transition -> Bool
isCancelling (Transition _ TransitionConfig{..}) = _transitionCancelling
isCancelling (Animation _ TransitionConfig{..}) = _transitionCancelling

-- | Transition event configuration
data TransitionConfig = TransitionConfig
  { _transitionDuration :: NominalDiffTime
  -- How long the css animation lasts for, ignored for 'Instant'
  , _transitionDirection :: Maybe Direction
  -- The final state of the element after an animation, and the direction of a
  -- transition. 'Nothing' toggles the current state for 'Transition' and leaves
  -- the current state for 'Animation'.
  , _transitionCancelling :: Bool
  -- Whether this transition event will override any that are queued or still
  -- occuring
  }
makeLenses ''TransitionConfig

instance Default TransitionConfig where
  def = TransitionConfig
    { _transitionDuration = 0.5 -- TODO: this is valid for transitions, but animations need a 0.75 default
    , _transitionDirection = Nothing
    , _transitionCancelling = False
    }

-- | Transition configuration for elements
data Action t = Action
  { _actionEvent :: Maybe (Event t Transition)
  , _actionInitialDirection :: Direction
  , _actionForceVisible :: Bool
  }

instance Reflex t => Default (Action t) where
  def = Action Nothing In False

instance Reflex t => Semigroup (Action t) where
  Action e1 i v <> Action e2 _ _ = Action (joinEvents e1 e2) i v

joinEvents
  :: Reflex t => Maybe (Event t a) -> Maybe (Event t a) -> Maybe (Event t a)
joinEvents (Just e1) (Just e2) = Just $ leftmost [e1, e2]
joinEvents e1 e2 = e1 <|> e2

actionEvent :: Lens' (Action t) (Maybe (Event t Transition))
actionEvent f (Action e d fv) = (\e' -> Action e' d fv) <$> f e

actionInitialDirection :: Lens' (Action t) Direction
actionInitialDirection f (Action e d fv)
  = (\d' -> Action e d' fv) <$> f d

actionForceVisible :: Lens' (Action t) Bool
actionForceVisible f (Action e d fv)
  = (\fv' -> Action e d fv') <$> f fv

-- | Queue of transitions
data Queue = Queue
  { _queueCanRun :: Bool
  -- ^ If the queue is ready to be run
  , _queueCurrentDirection :: Direction
  -- ^ The direction that the element will be in before the next request
  , _queueLastKey :: Int
  -- ^ Last id inserted
  , _queueLastCancel :: Int
  -- ^ Last id to cause a cancellation
  , _queueItems :: IntMap QueueItem
  -- ^ The actual queue
  }

-- | Get the first item from the queue along with its key
firstItem :: Queue -> Maybe (Int, QueueItem)
firstItem = fmap fst . IM.minViewWithKey . _queueItems

-- Get the transition / animation type
getTransType :: Transition -> Either TransitionType AnimationType
getTransType (Transition t _) = Left t
getTransType (Animation t _) = Right t

-- | 'QueueItem' is similar to 'Transition' but with a concrete start and end
-- direction.
data QueueItem = QueueItem
  { _queueItemTransType :: Either TransitionType AnimationType
  , _queueItemDuration :: NominalDiffTime
  , _queueItemStartDirection :: Direction
  , _queueItemEndDirection :: Direction
  }

-- | Flip the given direction
flipDirection :: Direction -> Direction
flipDirection In = Out
flipDirection Out = In

-- | Given a transition and the current/old direction, determine the users
-- intended final direction
determineEndDirection :: Transition -> Direction -> Direction
determineEndDirection t oldDirection
  | Just d <- getDirection t = d
  | Animation _ _ <- t = oldDirection
  | otherwise = flipDirection oldDirection

-- | Initial queue given the starting direction
initialQueue :: Direction -> Queue
initialQueue d = Queue
  { _queueCanRun = False
  , _queueCurrentDirection = d
  , _queueLastKey = 0
  , _queueLastCancel = 0
  , _queueItems = mempty
  }

-- | Update the queue when given 'This' 'Transition' (fired by the user) or a
-- 'That' '()' (denoting that the first transition has finished)
updateQueue :: These Transition () -> Queue -> Queue
updateQueue (This t) queue
  | isCancelling t = queue
    { _queueCanRun = True -- Cancel events can always run
    , _queueCurrentDirection = newDir
    , _queueLastKey = newKey
    , _queueLastCancel = newKey
    , _queueItems = IM.singleton newKey item
    }
  | otherwise = queue
    { _queueCanRun = IM.null $ _queueItems queue -- Only run when the old queue is empty
    , _queueCurrentDirection = newDir
    , _queueLastKey = newKey
    , _queueItems = IM.insert newKey item $ _queueItems queue
    }

  where newDir = determineEndDirection t $ _queueCurrentDirection queue
        newKey = _queueLastKey queue + 1
        item = QueueItem
          { _queueItemTransType = getTransType t
          , _queueItemDuration = getDuration t
          , _queueItemStartDirection = _queueCurrentDirection queue
          , _queueItemEndDirection = newDir
          }

-- Delete the first item when a finish event comes in
updateQueue (That ()) queue = queue
  { _queueCanRun = True -- Allow the next item to run
  , _queueItems = IM.deleteMin $ _queueItems queue
  }

-- Do both updates
updateQueue (These t ()) queue
  = updateQueue (This t) $ updateQueue (That ()) queue

-- | Run a transition, returning the classes and styles the element needs to
-- use.
runAction
  :: ( MonadFix m, MonadHold t m, TriggerEvent t m, PerformEvent t m
     , MonadIO (Performable m), Reflex t )
  => Action t
  -> m (AnimationAttrs t)
runAction (Action Nothing initDirection forceVisible) = do
  let mClasses = finalClasses forceVisible initDirection
  pure $ AnimationAttrs (pure mClasses) (pure Nothing)

runAction (Action (Just eTransition) initDirection forceVisible) = do

  rec
    -- Holds the queue and running state
    dTransitionQueue <- foldDyn updateQueue (initialQueue initDirection)
                      $ align eTransition (void eFinalFiltered)

    -- Fires when queue is updated and is able to run
    -- Contains the key and transition to run
    let eStart = fmapMaybe firstItem
               $ ffilter _queueCanRun
               $ updated dTransitionQueue

    -- This allows time for the class to update when an animation is finished,
    -- thereby restarting the animation if there is a queue. The best way (and
    -- the way semantic-ui does it) seems to be the trick mentioned here:
    -- https://css-tricks.com/restart-css-animation/
    -- This cheap and cheerful delay seems to work though.
    let minDuration = 0.05

    -- Delay the queue events by their duration, to signal when the transitions
    -- have ended
    eFinal <- performEventAsync $ ffor eStart $
      \(lastCancelledKey, item) cb -> case _queueItemTransType item of
        Left Instant -> liftIO $ void $ forkIO $ do
          -- Delay Instant items by the minDuration
          Concurrent.delay $ ceiling $ minDuration * 1000000
          cb (lastCancelledKey, _queueItemEndDirection item)
        _ -> liftIO $ void $ forkIO $ do
            -- Delay other items by at least minDuration
            Concurrent.delay $ ceiling $
              1000000 * max minDuration (_queueItemDuration item)
            cb (lastCancelledKey, _queueItemEndDirection item)

    -- Filter the finish events to remove animations which have been cancelled
    let filterCancelled queue (key, direction)
          = if _queueLastCancel queue > key then Nothing else Just direction
        eFinalFiltered = attachWithMaybe filterCancelled
                        (current dTransitionQueue) eFinal

  -- Duplicate the start event with a delay such that it always fires between
  -- the true start event and the earliest possible finish event.
  -- The true start event is used to clear animation classes to refresh an
  -- interrupted animation, and this delayed start event is used to set the
  -- new animation classes. Instant transitions are removed because they have
  -- no animation classes.
  eAnimStart <- delay (minDuration / 2)
              $ ffilter (\item -> _queueItemTransType item /= Left Instant)
              $ fmap snd eStart

  mClasses <- holdDyn (finalClasses forceVisible initDirection) $ leftmost
    -- Clear the possible existing classes by setting to the start direction
    [ finalClasses forceVisible . _queueItemStartDirection . snd <$> eStart
    -- Set any animating classes
    , animatingClasses forceVisible <$> eAnimStart
    -- Set the final classes
    , finalClasses forceVisible <$> eFinalFiltered ]

  mStyle <- holdDyn Nothing $ leftmost
    [ animatingStyle . snd <$> eStart
    , Nothing <$ eFinalFiltered ]

  pure $ AnimationAttrs (Dyn mClasses) (Dyn mStyle)

-- | Make the animating classes from a queue item
animatingClasses :: Bool -> QueueItem -> Maybe Classes
animatingClasses fv (QueueItem (Left t) _ _ d)
  = Just $ Classes
    [ "animating", "transition", toClassText t, toClassText d
    , if fv then "visible" else mempty ]
animatingClasses fv (QueueItem (Right t) _ _ _)
  = Just $ Classes
    [ "animating", "transition", toClassText t
    , if fv then "visible" else mempty ]

-- | Make the final classes given the direction and whether visibility should be
-- forced
finalClasses :: Bool -> Direction -> Maybe Classes
finalClasses _ Out = Just $ Classes ["transition", "hidden"]
finalClasses True In = Just $ Classes ["transition", "visible"]
finalClasses False In = Nothing

-- | Make the animating styles
animatingStyle :: QueueItem -> Maybe Style
animatingStyle (QueueItem _ d _ _)
  = Just $ Style $ "animation-duration: " <> tshow d


data ActiveElConfig t = ActiveElConfig
  { _classes :: Active t Classes
  , _style :: Active t Style
  , _attrs :: Active t (Map Text Text)
  , _action :: Maybe (Action t)
  }

instance Reflex t => Default (ActiveElConfig t) where
  def = ActiveElConfig
    { _classes = pure mempty
    , _style = pure mempty
    , _attrs = pure mempty
    , _action = Nothing
    }

-- Lenses

elConfigTransition :: Lens' (ActiveElConfig t) (Maybe (Action t))
elConfigTransition f (ActiveElConfig c s at t)
  = ActiveElConfig c s at <$> f t

elConfigAttributes
  :: Lens' (ActiveElConfig t) (Active t (Map Text Text))
elConfigAttributes f (ActiveElConfig c s at t)
  = (\at' -> ActiveElConfig c s at' t) <$> f at

elConfigStyle :: Lens' (ActiveElConfig t) (Active t Style)
elConfigStyle f (ActiveElConfig c s at t)
  = (\s' -> ActiveElConfig c s' at t) <$> f s

elConfigClasses :: Lens' (ActiveElConfig t) (Active t Classes)
elConfigClasses f (ActiveElConfig c s at t)
  = (\c' -> ActiveElConfig c' s at t) <$> f c

class HasAction t a | a -> t where
  action :: Lens' a (Maybe (Action t))

class HasAttributes t a | a -> t where
  attrs :: Lens' a (Active t (Map Text Text))

class HasStyle t a | a -> t where
  style :: Lens' a (Active t Style)

class HasClasses t a | a -> t where
  classes :: Lens' a (Active t Classes)

class HasElConfig t a | a -> t where
  elConfig :: Lens' a (ActiveElConfig t)

instance HasElConfig t (ActiveElConfig t) where
  elConfig = id

instance HasElConfig t a => HasAttributes t a where
  attrs = elConfig . elConfigAttributes

instance HasElConfig t a => HasStyle t a where
  style = elConfig . elConfigStyle

instance HasElConfig t a => HasClasses t a where
  classes = elConfig . elConfigClasses

instance HasElConfig t a => HasAction t a where
  action = elConfig . elConfigTransition

-- | Left biased
instance Reflex t => Semigroup (ActiveElConfig t) where
  ActiveElConfig a b c d <> ActiveElConfig a' b' c' d' = ActiveElConfig
    (a `mappend` a') (b `mappend` b') (c `mappend` c') (d <> d')

instance Reflex t => Monoid (ActiveElConfig t) where
  mempty = def
  mappend = (<>)

type UI t m =
  ( MonadHold t m, TriggerEvent t m, PerformEvent t m, DomBuilder t m
  , PostBuild t m, MonadIO (Performable m), MonadFix m, Reflex t )

{-# INLINABLE ui #-}
ui :: UI t m => Text -> ActiveElConfig t -> m a -> m a
ui elTag conf = fmap snd . ui' elTag conf

{-# INLINABLE ui' #-}
ui'
  :: UI t m => Text -> ActiveElConfig t -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
ui' elTag ActiveElConfig {..} child = do
  AnimationAttrs dMClasses dMStyle <- runAction $ fromMaybe def _action
  let mkAttrs c mClasses s mStyle as = sconcat
        [ classAttr $ maybe c (<> c) mClasses
        , styleAttr $ maybe s (<> s) mStyle
        , as ]
      dynAttrs
        = mkAttrs <$> _classes <*> dMClasses <*> _style <*> dMStyle <*> _attrs
  case dynAttrs of
    Dyn a -> elDynAttr' elTag a child
    Static a -> elAttr' elTag a child

data SetValue' t a b = SetValue
  { _initial :: a
  , _event :: Maybe (Event t b)
  }
makeLenses ''SetValue'

type SetValue t a = SetValue' t a a

instance Reflex t => Semigroup (SetValue' t a b) where
  SetValue a mEvt1 <> SetValue _ mEvt2 = SetValue a (joinEvents mEvt1 mEvt2)

