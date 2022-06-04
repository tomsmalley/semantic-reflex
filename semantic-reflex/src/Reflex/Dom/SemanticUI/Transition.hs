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
  , TransitionType (..)
  , TransitionConfig (..)
  , transitionConfig_duration
  , transitionConfig_cancelling

  -- * Animation
  , Animation (..)
  , AnimationType (..)
  , AnimationConfig (..)

  -- * Action
  , Action (..)
  , forceVisible
  , transitionStateClasses
  , TransitionState (..)

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

import Control.Lens.TH (makeLenses)

import Control.Applicative (Alternative(..))
import Control.Concurrent
import Control.Lens (Lens')
import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans (liftIO)

import Data.Align
import Data.Default (Default(..))
import Data.Either (isRight)
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
  | Zoom
  | Fade | FadeUp | FadeDown | FadeLeft | FadeRight
  | HorizontalFlip | VerticalFlip
  | Drop
  | FlyLeft | FlyRight | FlyUp | FlyDown
  | SwingLeft | SwingRight | SwingUp | SwingDown
  | Browse | BrowseRight
  | SlideDown | SlideUp | SlideLeft | SlideRight
  | CustomTransition Text
  deriving (Eq, Show)

instance Default TransitionType where
  def = Fade

instance ToClassText TransitionType where
  toClassText Instant = ""
  toClassText Scale = "scale"
  toClassText Zoom = "zoom"
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
  toClassText (CustomTransition classes) = classes

-- | Animation types as listed in
-- https://semantic-ui.com/modules/transition.html#static-animations
data AnimationType = Jiggle | Flash | Shake | Pulse | Tada | Bounce | Glow
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToClassText AnimationType where
  toClassText Jiggle = "jiggle"
  toClassText Flash = "flash"
  toClassText Shake = "shake"
  toClassText Pulse = "pulse"
  toClassText Tada = "tada"
  toClassText Bounce = "bounce"
  toClassText Glow = "glow"

data Direction = In | Out
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToClassText Direction where
  toClassText In = "in"
  toClassText Out = "out"

-- | Returned by 'runAction', controls the relavant animation / transition
-- classes and styles.
data AnimationAttrs t = AnimationAttrs
  { _animationAttrs_class :: Active t (Maybe Classes)
  , _animationAttrs_style :: Active t (Maybe Style)
  }

data Animation = Animation AnimationType AnimationConfig
data Transition = Transition TransitionType (Maybe Direction) TransitionConfig

data AnimationConfig = AnimationConfig
  { _animationConfig_duration :: NominalDiffTime
  -- ^ Duration of the animation
  , _animationConfig_cancelling :: Bool
  -- ^ Whether this animation should override any animations that are queued or still occuring
  }

instance Default AnimationConfig where
  def = AnimationConfig 0.75 False

-- | Transition event configuration
data TransitionConfig = TransitionConfig
  { _transitionConfig_duration :: NominalDiffTime
  -- ^ How long the animation lasts for, ignored for 'Instant'
  , _transitionConfig_cancelling :: Bool
  -- ^ Whether this transition event will override any animations that are queued or still
  -- occuring
  } deriving Show
makeLenses ''TransitionConfig

instance Default TransitionConfig where
  def = TransitionConfig
    { _transitionConfig_duration = 0.5
    , _transitionConfig_cancelling = False
    }

getDirection :: Either Transition Animation -> Maybe Direction
getDirection (Left (Transition _ md _)) = md
getDirection (Right _) = Nothing

getDuration :: Either Transition Animation -> NominalDiffTime
getDuration (Left (Transition Instant _ _)) = 0
getDuration (Left (Transition _ _ TransitionConfig{..})) = _transitionConfig_duration
getDuration (Right (Animation _ AnimationConfig{..})) = _animationConfig_duration

isCancelling :: Either Transition Animation -> Bool
isCancelling (Left (Transition _ _ TransitionConfig{..})) = _transitionConfig_cancelling
isCancelling (Right (Animation _ AnimationConfig{..})) = _animationConfig_cancelling

getTransType :: Either Transition Animation -> Either TransitionType AnimationType
getTransType (Left (Transition t _ _)) = Left t
getTransType (Right (Animation t _)) = Right t


-- | Transition configuration for elements
data Action t = Action
  { _action_initialDirection :: Direction
  -- ^ Initial direction of the element
  , _action_transition :: Event t Transition
  -- ^ Transition events
  , _action_animation :: Event t Animation
  -- ^ Animation events
  , _action_transitionStateClasses :: Bool -> Text -> Direction -> TransitionState -> Maybe Classes
  -- ^ A function to control the classes at various stages of
  -- animation/transition state. The first argument is 'False' for transitions,
  -- and 'True' for animations. The second contains the class text of the
  -- current or last animation.
  }

data TransitionState
  = TransitionState_Animating
  | TransitionState_Final

transitionStateClasses :: Bool -> Text -> Direction -> TransitionState -> Maybe Classes
transitionStateClasses isAnimation lastClasses direction = \case
  TransitionState_Animating -> Just $ Classes $
    "animating" : "transition" : lastClasses : if isAnimation then [] else [toClassText direction]
  TransitionState_Final -> case direction of
      Out -> Just $ Classes ["transition", "hidden"]
      In -> Nothing

forceVisible :: Bool -> Text -> Direction -> TransitionState -> Maybe Classes
forceVisible isAnimation lastClass direction = \case
  TransitionState_Animating -> Just $ Classes $
    "animating" : "transition" : "visible" : lastClass : if isAnimation then [] else [toClassText direction]
  TransitionState_Final -> case direction of
    Out -> Just $ Classes ["transition", "hidden"]
    In -> Just $ Classes ["transition", "visible"]

instance Reflex t => Default (Action t) where
  def = Action In never never transitionStateClasses

instance Reflex t => Semigroup (Action t) where
  Action i t1 e1 f <> Action _i t2 e2 _ = Action i (leftmost [t1, t2]) (leftmost [e1, e2]) f

joinEvents
  :: Reflex t => Maybe (Event t a) -> Maybe (Event t a) -> Maybe (Event t a)
joinEvents (Just e1) (Just e2) = Just $ leftmost [e1, e2]
joinEvents e1 e2 = e1 <|> e2

-- | Queue of transitions
data Queue = Queue
  { queue_currentDirection :: Direction
  -- ^ The direction that the element will be in before the next request
  , queue_lastInit :: Int
  -- ^ Last id whose transition was initialised
  , queue_lastId :: Int
  -- ^ Last id added (used to ensure unique ids)
  , queue_items :: IntMap QueueItem
  -- ^ The actual queue
  } deriving Show

-- | 'QueueItem' is similar to 'TransitionOrAnimation' but with a concrete start and end
-- direction.
data QueueItem = QueueItem
  { queueItem_transType :: Either TransitionType AnimationType
  , queueItem_duration :: NominalDiffTime
  , queueItem_startDirection :: Direction
  , queueItem_endDirection :: Direction
  } deriving Show

-- | Flip the given direction
flipDirection :: Direction -> Direction
flipDirection In = Out
flipDirection Out = In

-- | Given a transition and the current/old direction, determine the users
-- intended final direction
determineEndDirection :: Either Transition Animation -> Direction -> Direction
determineEndDirection t oldDirection
  | Just d <- getDirection t = d
  | Right _ <- t = oldDirection
  | otherwise = flipDirection oldDirection

-- | Initial queue given the starting direction
initialQueue :: Direction -> Queue
initialQueue d = Queue
  { queue_currentDirection = d
  , queue_lastInit = -1
  , queue_lastId = 0
  , queue_items = mempty
  }

-- | The transition state is really 'Start' or 'Finish' - that is, it is
-- currently animating or it is in its final state. The purpose of 'Init' is to
-- allow time for the class to update when an animation is finished,
-- thereby restarting the animation if there is a queue. The best way (and
-- the way semantic-ui does it) seems to be the trick mentioned here:
-- https://css-tricks.com/restart-css-animation/
-- This cheap and cheerful delay seems to work though.
data State = Init | Start | Finish deriving Show

-- | Run a transition, returning the classes and styles the element needs to use.
runAction
  :: (MonadFix m, MonadHold t m, TriggerEvent t m, PerformEvent t m, Prerender t m)
  => Action t -> m (AnimationAttrs t)
runAction (Action initDirection transitionRequest animationRequest mkClasses) = do
  let request = leftmost [Left <$> transitionRequest, Right <$> animationRequest]

  rec
    (_, transition) <- mapAccumMaybeB handle (initialQueue initDirection) $ align request delayed
    delayed <- fmap (switch . current) $ prerender (pure never) $ performEventAsync $ fforMaybe transition $ \(s, tid, i) -> case s of
      -- Delay the init events by a small time before signalling a transition to start
      Init -> Just $ \cb -> liftIO $ void $ forkIO $ do
        Concurrent.delay 20000
        cb (Start, tid, i)
      -- Delay the start events by their duration before signalling when the transitions have ended
      Start -> Just $ \cb -> liftIO $ void $ forkIO $ do
        Concurrent.delay $ ceiling $ 1000000 * queueItem_duration i
        cb (Finish, tid, i)
      Finish -> Nothing

  st <- foldDyn ($) (False, "", initDirection, TransitionState_Final) $ ffor transition $ \(s, _, qi) -> case s of
    Init -> \(isAnim, t, _, _) -> (isAnim, t, queueItem_startDirection qi, TransitionState_Final)
    Start -> \_ ->
      ( isRight $ queueItem_transType qi
      , either toClassText toClassText $ queueItem_transType qi
      , queueItem_endDirection qi
      , TransitionState_Animating
      )
    Finish -> \(isAnim, t, _, _) ->  (isAnim, t, queueItem_endDirection qi, TransitionState_Final)

  mStyle <- holdDyn Nothing $ fforMaybe transition $ \(s, _, t) -> case s of
    Init -> Just $ animatingStyle t
    Start -> Nothing
    Finish -> Just Nothing

  pure $ AnimationAttrs (Dyn $ (\(a,b,c,d) -> mkClasses a b c d) <$> st) (Dyn mStyle)

  where
    handle
      :: Queue
      -> These (Either Transition Animation) (State, Int, QueueItem)
      -> (Maybe Queue, Maybe (State, Int, QueueItem))
    handle queue = \case
      This t -- Requests to start a transition
        -- When the queue is clear, or the transition is cancelling, we can run immediately
        | null (queue_items queue) || isCancelling t ->
          ( Just $ newQueue { queue_items = IM.singleton newId item, queue_lastInit = newId }
          , Just (Init, newId, item) )
        | otherwise -> -- Otherwise just add the item to the queue
          ( Just $ newQueue { queue_items = IM.insert newId item $ queue_items queue }
          , Nothing )
        where newDir = determineEndDirection t $ queue_currentDirection queue
              newQueue = queue { queue_currentDirection = newDir, queue_lastId = newId }
              newId = succ $ queue_lastId queue
              item = QueueItem
                { queueItem_transType = getTransType t
                , queueItem_duration = getDuration t
                , queueItem_startDirection = queue_currentDirection queue
                , queueItem_endDirection = newDir
                }

      That (Init, _, _) -> (Nothing, Nothing) -- Should never happen
      That (Start, tid, t) -> -- Transition start events, block them if a newer transition has started
        (Nothing, if queue_lastInit queue > tid then Nothing else Just (Start, tid, t))
      That (Finish, tid, t) -> -- Transition finish events
        ( Just $ queue { queue_items = newQueueItems } -- Always remove finished transitions from the queue
        , if queue_lastInit queue > tid -- If a newer transition has started, block the event
          then Nothing
          else Just $ case lookupMin newQueueItems of
            Just (qid, qt) -> (Init, qid, qt) -- If there is a queued transition we start it immediately
            Nothing -> (Finish, tid, t) ) -- Otherwise let the finish event through
        where newQueueItems = IM.delete tid $ queue_items queue

      -- When both a request and a finish event happen at the same time, handle the finish event and
      -- feed that resultant queue into the handler for the request event. Prefer the resultant
      -- request event over the resultant finish event.
      These req ret -> let (finishQueue, finishEvent) = handle queue $ That ret
                           (requestQueue, requestEvent) = handle (fromMaybe queue finishQueue) $ This req
                        in (requestQueue, requestEvent <|> finishEvent)

-- | Safely retrieve the minimal key of an 'IntMap'
lookupMin :: IntMap a -> Maybe (Int, a)
lookupMin = fmap fst . IM.minViewWithKey

-- | Make the animating styles
animatingStyle :: QueueItem -> Maybe Style
animatingStyle (QueueItem _ d _ _)
  = Just $ Style $ "animation-duration: " <> tshow d

-- | Configuration for semantic UI elements
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
  , PostBuild t m, Prerender t m, MonadFix m, Reflex t )

-- | Similar to 'el' but for animatable things from Semantic UI
ui :: UI t m => Text -> ActiveElConfig t -> m a -> m a
ui elTag conf = fmap snd . ui' elTag conf
{-# INLINABLE ui #-}

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
{-# INLINABLE ui' #-}

data SetValue' t a b = SetValue
  { _initial :: a
  , _event :: Maybe (Event t b)
  }
makeLenses ''SetValue'

type SetValue t a = SetValue' t a a

instance Reflex t => Semigroup (SetValue' t a b) where
  SetValue a mEvt1 <> SetValue _ mEvt2 = SetValue a (joinEvents mEvt1 mEvt2)
